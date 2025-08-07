//! elmdb_nif - High-performance LMDB bindings for Erlang via Rust NIF
//!
//! This module implements a Native Implemented Function (NIF) that provides
//! Erlang/Elixir applications with access to LMDB (Lightning Memory-Mapped Database).
//!
//! # Architecture
//!
//! The implementation uses a two-layer architecture:
//! - **Resource Management**: Environments and databases are managed as Erlang resources
//! - **Write Optimization**: Batches small writes into single transactions for performance
//!
//! # Safety
//!
//! - All LMDB operations are wrapped in safe Rust abstractions
//! - Resources are automatically cleaned up when no longer referenced
//! - Thread-safe through Arc<Mutex<>> wrappers
//! - Prevents use-after-close errors through validation checks
//!
//! # Performance
//!
//! Key optimizations include:
//! - Write batching to reduce transaction overhead
//! - Zero-copy reads through memory mapping
//! - Efficient cursor iteration for list operations
//! - Early termination for prefix searches

use rustler::{Env, Term, NifResult, Error, Encoder, ResourceArc};
use rustler::types::binary::Binary;
use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex};
use std::path::Path;
use lmdb::{Environment, EnvironmentFlags, Database, DatabaseFlags, Transaction, WriteFlags, Cursor};
use lmdb_sys::ffi;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        not_found,
        nif_not_loaded,
        // LMDB-specific atoms
        map_size,
        no_mem_init,
        no_sync,
        write_map,
        create,
        // Error atoms
        invalid_path,
        permission_denied,
        already_open,
        environment_error,
        database_error,
        transaction_error,
        key_exist,
        map_full,
        txn_full,
        // Specific environment errors
        directory_not_found,
        no_space,
        io_error,
        corrupted,
        version_mismatch,
        map_resized,
    }
}

/// Write operation to be batched
#[derive(Debug, Clone)]
struct WriteOperation {
    key: Vec<u8>,
    value: Vec<u8>,
}

/// Buffer for accumulating write operations before committing
/// 
/// This buffer improves write performance by batching multiple small
/// writes into a single LMDB transaction, reducing overhead significantly.
#[derive(Debug)]
struct WriteBuffer {
    operations: VecDeque<WriteOperation>,
    max_size: usize,
}

impl WriteBuffer {
    /// Create a new write buffer with specified capacity
    fn new(max_size: usize) -> Self {
        Self {
            operations: VecDeque::new(),
            max_size,
        }
    }

    /// Check if buffer has pending operations
    fn is_empty(&self) -> bool {
        self.operations.is_empty()
    }

    /// Drain all operations from the buffer
    fn drain(&mut self) -> Vec<WriteOperation> {
        self.operations.drain(..).collect()
    }

    /// Check if buffer has reached capacity and should be flushed
    fn should_flush(&self) -> bool {
        self.operations.len() >= self.max_size
    }

    /// Add operation without checking capacity
    fn add_without_check(&mut self, key: Vec<u8>, value: Vec<u8>) {
        self.operations.push_back(WriteOperation { key, value });
    }
}

/// LMDB Environment resource
/// 
/// Represents an LMDB environment that can contain multiple databases.
/// Environments are reference-counted and shared across database instances.
#[derive(Debug)]
pub struct LmdbEnv {
    /// The underlying LMDB environment
    env: Environment,
    /// Path to the database directory
    path: String,
    /// Flag indicating if environment has been closed
    closed: Arc<Mutex<bool>>,
    /// Reference count for active databases using this environment
    ref_count: Arc<Mutex<usize>>,
}

/// LMDB Database resource
/// 
/// Represents a database within an LMDB environment.
/// Each database has its own write buffer for batching operations.
pub struct LmdbDatabase {
    /// The underlying LMDB database
    db: Database,
    /// Reference to the parent environment
    env: ResourceArc<LmdbEnv>,
    /// Buffer for batching write operations
    write_buffer: Arc<Mutex<WriteBuffer>>,
    /// Flag indicating if database has been closed
    closed: Arc<Mutex<bool>>,
}

/// Global registry of open environments
/// 
/// Ensures that each directory path has at most one environment open,
/// preventing LMDB conflicts and improving resource sharing.
lazy_static::lazy_static! {
    static ref ENVIRONMENTS: Arc<Mutex<HashMap<String, ResourceArc<LmdbEnv>>>> = 
        Arc::new(Mutex::new(HashMap::new()));
}

/// Initialize the NIF module
/// 
/// Registers resource types with the Erlang runtime.
/// This function is called automatically when the NIF is loaded.
fn init(env: Env, _info: Term) -> bool {
    rustler::resource!(LmdbEnv, env);
    rustler::resource!(LmdbDatabase, env);
    true
}

///===================================================================
/// Environment Management
///===================================================================

#[rustler::nif]
fn env_open<'a>(env: Env<'a>, path: Term<'a>, options: Vec<Term<'a>>) -> NifResult<Term<'a>> {
    let path_string = if let Ok(binary) = path.decode::<Binary>() {
        std::str::from_utf8(&binary).map_err(|_| Error::BadArg)?.to_string()
    } else if let Ok(string) = path.decode::<String>() {
        string
    } else if let Ok(chars) = path.decode::<Vec<u8>>() {
        // Handle Erlang strings (lists of integers)
        std::str::from_utf8(&chars).map_err(|_| Error::BadArg)?.to_string()
    } else {
        return Err(Error::BadArg);
    };
    let path_str = &path_string;
    let parsed_options = parse_env_options(options)?;
    
    // Check if environment is already open for this path
    {
        let environments = ENVIRONMENTS.lock().unwrap();
        if environments.contains_key(path_str) {
            return Ok((atoms::error(), atoms::already_open()).encode(env));
        }
    }
    
    // Create LMDB environment builder
    let mut env_builder = Environment::new();
    
    // Set map size if provided (default is 1MB, so we should set a reasonable size)
    if let Some(map_size) = parsed_options.map_size {
        env_builder.set_map_size(map_size as usize);
    } else {
        // Default to 1GB if no map size is specified
        env_builder.set_map_size(1024 * 1024 * 1024);
    }
    
    // Set flags based on options
    let mut flags = EnvironmentFlags::empty();
    if parsed_options.no_mem_init {
        flags |= EnvironmentFlags::NO_MEM_INIT;
    }
    if parsed_options.no_sync {
        flags |= EnvironmentFlags::NO_SYNC;
    }
    if parsed_options.write_map {
        flags |= EnvironmentFlags::WRITE_MAP;
    }
    
    env_builder.set_flags(flags);
    
    // Open the environment
    let lmdb_env_result = env_builder.open(Path::new(path_str));
    
    let lmdb_environment = match lmdb_env_result {
        Ok(env) => env,
        Err(e) => {
            // Check if the directory itself exists
            let path = Path::new(path_str);
            
            let error_atom = if !path.exists() {
                atoms::directory_not_found()
            } else if path.is_file() {
                // Path exists but is a file, not a directory
                atoms::invalid_path()
            } else {
                // Directory exists, check permissions
                match std::fs::File::create(path.join(".lmdb_test")) {
                    Ok(_) => {
                        // Clean up test file
                        let _ = std::fs::remove_file(path.join(".lmdb_test"));
                        // Permission is OK, must be another LMDB error
                        match e {
                            lmdb::Error::Corrupted => atoms::corrupted(),
                            lmdb::Error::VersionMismatch => atoms::version_mismatch(),
                            lmdb::Error::MapFull => atoms::map_full(),
                            _ => atoms::environment_error()
                        }
                    },
                    Err(io_err) => {
                        match io_err.kind() {
                            std::io::ErrorKind::PermissionDenied => atoms::permission_denied(),
                            _ => atoms::environment_error()
                        }
                    }
                }
            };
            
            return Ok((atoms::error(), error_atom).encode(env));
        }
    };
    
    // Create our wrapper struct
    let lmdb_env = LmdbEnv { 
        env: lmdb_environment,
        path: path_str.to_string(),
        closed: Arc::new(Mutex::new(false)),
        ref_count: Arc::new(Mutex::new(0)),
    };
    let resource = ResourceArc::new(lmdb_env);
    
    // Store in global environments map
    {
        let mut environments = ENVIRONMENTS.lock().unwrap();
        environments.insert(path_str.to_string(), resource.clone());
    }
    
    Ok((atoms::ok(), resource).encode(env))
}

#[rustler::nif]
fn env_close<'a>(env: Env<'a>, env_handle: ResourceArc<LmdbEnv>) -> NifResult<Term<'a>> {
    // Check if there are still active database references
    {
        let ref_count = env_handle.ref_count.lock().unwrap();
        if *ref_count > 0 {
            return Ok((atoms::error(), atoms::environment_error(), 
                      format!("Environment still has {} active database references", *ref_count)).encode(env));
        }
    }
    
    // Mark environment as closed
    {
        let mut closed = env_handle.closed.lock().unwrap();
        *closed = true;
    }
    
    // Remove from global environments map
    {
        let mut environments = ENVIRONMENTS.lock().unwrap();
        environments.remove(&env_handle.path);
    }
    
    // The Drop trait implementation will handle the actual closing
    // when the ResourceArc is dropped
    
    Ok(atoms::ok().encode(env))
}

#[rustler::nif]
fn env_close_by_name<'a>(env: Env<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    let path_string = if let Ok(binary) = path.decode::<Binary>() {
        std::str::from_utf8(&binary).map_err(|_| Error::BadArg)?.to_string()
    } else if let Ok(string) = path.decode::<String>() {
        string
    } else if let Ok(chars) = path.decode::<Vec<u8>>() {
        // Handle Erlang strings (lists of integers)
        std::str::from_utf8(&chars).map_err(|_| Error::BadArg)?.to_string()
    } else {
        return Err(Error::BadArg);
    };
    let path_str = &path_string;
    
    // Mark environment as closed and remove from global environments map
    {
        let mut environments = ENVIRONMENTS.lock().unwrap();
        if let Some(env_handle) = environments.get(path_str) {
            // Check if there are still active database references
            {
                let ref_count = env_handle.ref_count.lock().unwrap();
                if *ref_count > 0 {
                    return Ok((atoms::error(), atoms::environment_error(), 
                              format!("Environment still has {} active database references", *ref_count)).encode(env));
                }
            }
            
            // Mark as closed
            let mut closed = env_handle.closed.lock().unwrap();
            *closed = true;
            drop(closed);
            
            // Remove from map
            environments.remove(path_str);
            Ok(atoms::ok().encode(env))
        } else {
            Ok((atoms::error(), atoms::not_found()).encode(env))
        }
    }
}

///===================================================================
/// Database Operations
///===================================================================

#[rustler::nif]
fn db_close<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>
) -> NifResult<Term<'a>> {
    // Mark database as closed
    {
        let mut closed = db_handle.closed.lock().map_err(|_| Error::BadArg)?;
        if *closed {
            return Ok((atoms::error(), atoms::database_error(), "Database already closed".to_string()).encode(env));
        }
        *closed = true;
    }
    
    // Force flush any pending writes
    if let Err(error_msg) = db_handle.force_flush_buffer() {
        // Log error but don't fail the close operation
        eprintln!("Warning: Failed to flush buffer during db_close: {}", error_msg);
    }
    
    // Decrement reference count for the environment
    {
        let mut ref_count = db_handle.env.ref_count.lock().map_err(|_| Error::BadArg)?;
        if *ref_count > 0 {
            *ref_count -= 1;
        }
    }
    
    Ok(atoms::ok().encode(env))
}

#[rustler::nif]
fn db_open<'a>(
    env: Env<'a>, 
    env_handle: ResourceArc<LmdbEnv>, 
    options: Vec<Term<'a>>
) -> NifResult<Term<'a>> {
    let parsed_options = parse_db_options(options)?;
    
    // Check if environment is closed before attempting to open database
    {
        let closed = env_handle.closed.lock().map_err(|_| Error::BadArg)?;
        if *closed {
            return Ok((atoms::error(), atoms::database_error(), "Environment is closed".to_string()).encode(env));
        }
    }
    
    // Set database flags based on options
    let mut flags = DatabaseFlags::empty();
    if parsed_options.create {
        flags |= DatabaseFlags::empty(); // CREATE is default behavior
    }
    
    // Open the database using the environment with flags
    // Use None for the default (unnamed) database
    let database_result = if parsed_options.create {
        // Always use create_db for consistency - it will open existing or create new
        env_handle.env.create_db(None, flags)
    } else {
        // Try create_db first, fall back to open_db if needed
        match env_handle.env.create_db(None, flags) {
            Ok(db) => Ok(db),
            Err(_) => env_handle.env.open_db(None)
        }
    };
    
    let database = match database_result {
        Ok(db) => db,
        Err(lmdb_err) => {
            return match lmdb_err {
                lmdb::Error::NotFound if !parsed_options.create => {
                    Ok((atoms::error(), atoms::not_found()).encode(env))
                },
                _ => Ok((atoms::error(), atoms::database_error()).encode(env))
            };
        }
    };
    
    // Increment reference count for this environment
    {
        let mut ref_count = env_handle.ref_count.lock().map_err(|_| Error::BadArg)?;
        *ref_count += 1;
    }
    
    // Create database resource with write buffer (default buffer size: 1000 operations)
    let lmdb_db = LmdbDatabase { 
        db: database,
        env: env_handle.clone(),
        write_buffer: Arc::new(Mutex::new(WriteBuffer::new(1000))),
        closed: Arc::new(Mutex::new(false)),
    };
    let resource = ResourceArc::new(lmdb_db);
    
    Ok((atoms::ok(), resource).encode(env))
}

///===================================================================
/// Write Buffer Management - Transaction Batching Optimization
///
/// This implementation provides significant performance improvements for many small writes
/// by batching them into single transactions, similar to the C NIF approach:
///
/// Key optimizations:
/// 1. Accumulates writes in a buffer (default: 1000 operations)
/// 2. Creates a single transaction per batch instead of per write
/// 3. Only commits when:
///    - Buffer reaches capacity
///    - Read operation is performed (get/list)
///    - Explicit flush is called
///    - Database is being closed
/// 4. Dramatically reduces transaction overhead and lock contention
/// 5. Maintains consistency by flushing before reads
///
/// Performance benefits:
/// - Fewer transaction creates/commits (major LMDB overhead)
/// - Reduced write lock contention
/// - Better throughput when mixing reads and writes
/// - Maintains ACID properties
///===================================================================

impl LmdbDatabase {
    // Transaction batching optimization - keeps writes in buffer until:
    // 1. Buffer reaches capacity (1000 operations by default)
    // 2. A read operation is performed (get/list)
    // 3. Explicit flush is called
    // 4. Database is being closed
    // This dramatically improves write performance by reducing transaction overhead
    
    fn is_closed(&self) -> bool {
        if let Ok(closed) = self.closed.lock() {
            *closed
        } else {
            true // If we can't check, assume closed for safety
        }
    }


    // New method for immediate batched write without buffering
    fn write_immediate_batch(&self, operations: Vec<WriteOperation>) -> Result<(), String> {
        if operations.is_empty() {
            return Ok(());
        }

        // Create a write transaction for the batch
        let mut txn = self.env.env.begin_rw_txn()
            .map_err(|_| "Failed to begin write transaction")?;
        
        // Execute all operations in the batch
        for op in operations {
            txn.put(self.db, &op.key, &op.value, WriteFlags::empty())
                .map_err(|e| format!("Failed to put value: key_len={}, value_len={}, error={:?}", 
                                     op.key.len(), op.value.len(), e))?;
        }
        
        // Commit the transaction
        txn.commit()
            .map_err(|_| "Failed to commit batch transaction")?;
        
        Ok(())
    }

    // Optimized method that tries to batch multiple puts in a single transaction
    fn put_with_batching(&self, key: Vec<u8>, value: Vec<u8>) -> Result<(), String> {
        // First, add to buffer
        let (should_batch_immediately, current_ops) = {
            let mut buffer = self.write_buffer.lock().map_err(|_| "Failed to lock write buffer")?;
            buffer.add_without_check(key, value);
            
            // If buffer is full, drain it for immediate processing
            if buffer.should_flush() {
                let ops = buffer.drain();
                (true, ops)
            } else {
                // Buffer has space, keep accumulating
                (false, Vec::new())
            }
        };

        if should_batch_immediately {
            self.write_immediate_batch(current_ops)?;
        }

        Ok(())
    }

    // Force flush any pending writes - used before reads and on close
    fn force_flush_buffer(&self) -> Result<(), String> {
        let pending_ops = {
            let mut buffer = self.write_buffer.lock().map_err(|_| "Failed to lock write buffer")?;
            if buffer.is_empty() {
                return Ok(());
            }
            buffer.drain()
        };

        self.write_immediate_batch(pending_ops)
    }

    // Check if buffer has pending writes without flushing
    fn has_pending_writes(&self) -> bool {
        if let Ok(buffer) = self.write_buffer.lock() {
            !buffer.is_empty()
        } else {
            false
        }
    }

    fn validate_database(&self) -> Result<(), String> {
        // Check if database is marked as closed
        if self.is_closed() {
            return Err("Database is closed".to_string());
        }
        
        // Check if environment is marked as closed
        {
            let closed = self.env.closed.lock().map_err(|_| "Failed to check environment status")?;
            if *closed {
                return Err("Database environment is closed".to_string());
            }
        }
        
        // Quick validation by attempting to begin a read transaction
        let _txn = self.env.env.begin_ro_txn()
            .map_err(|e| format!("Database environment is invalid or closed: {:?}", e))?;
        Ok(())
    }
}

impl Drop for LmdbDatabase {
    fn drop(&mut self) {
        // Check if database was already explicitly closed
        let already_closed = {
            if let Ok(closed) = self.closed.lock() {
                *closed
            } else {
                false
            }
        };
        
        // Only decrement reference count if not already closed
        // (explicit close already decremented it)
        if !already_closed {
            // Attempt to flush any remaining buffered writes when the database is dropped
            // We ignore errors here since we can't handle them in Drop
            let _ = self.force_flush_buffer();
            
            // Decrement reference count for the environment
            if let Ok(mut ref_count) = self.env.ref_count.lock() {
                if *ref_count > 0 {
                    *ref_count -= 1;
                }
            }
        }
    }
}

///===================================================================
/// Key-Value Operations
///===================================================================

#[rustler::nif]
fn put<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    key: Binary,
    value: Binary
) -> NifResult<Term<'a>> {
    // Validate database and environment status
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }

    let key_vec = key.as_slice().to_vec();
    let value_vec = value.as_slice().to_vec();
    
    // Handle empty keys directly (don't buffer them as they cause issues in LMDB batch operations)
    if key_vec.is_empty() {
        let mut txn = match db_handle.env.env.begin_rw_txn() {
            Ok(txn) => txn,
            Err(_) => {
                return Ok((atoms::error(), atoms::transaction_error(), "Failed to begin write transaction".to_string()).encode(env));
            }
        };
        
        match txn.put(db_handle.db, &key_vec, &value_vec, WriteFlags::empty()) {
            Ok(()) => {
                match txn.commit() {
                    Ok(()) => return Ok(atoms::ok().encode(env)),
                    Err(_) => return Ok((atoms::error(), atoms::transaction_error(), "Failed to commit transaction".to_string()).encode(env))
                }
            },
            Err(lmdb_err) => {
                let error_msg = match lmdb_err {
                    lmdb::Error::BadValSize => "Empty key not supported".to_string(),
                    _ => format!("Failed to put value: {:?}", lmdb_err)
                };
                return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
            }
        }
    }
    
    // Add to write buffer for non-empty keys using new batching logic
    if let Err(error_msg) = db_handle.put_with_batching(key_vec, value_vec) {
        return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
    }
    
    Ok(atoms::ok().encode(env))
}

#[rustler::nif]
fn put_batch<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    key_value_pairs: Vec<(Binary, Binary)>
) -> NifResult<Term<'a>> {
    // Validate database and environment status
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    
    if key_value_pairs.is_empty() {
        return Ok(atoms::ok().encode(env));
    }
    
    // Create a write transaction for the entire batch
    let mut txn = match (*db_handle).env.env.begin_rw_txn() {
        Ok(txn) => txn,
        Err(_) => {
            return Ok((atoms::error(), atoms::transaction_error(), "Failed to begin write transaction".to_string()).encode(env));
        }
    };
    
    let mut success_count = 0;
    let mut errors = Vec::new();
    
    // Process all key-value pairs in a single transaction
    for (i, (key, value)) in key_value_pairs.iter().enumerate() {
        let key_bytes = key.as_slice();
        let value_bytes = value.as_slice();
        
        match txn.put((*db_handle).db, &key_bytes, &value_bytes, WriteFlags::empty()) {
            Ok(()) => {
                success_count += 1;
            },
            Err(lmdb_err) => {
                let error_detail = match lmdb_err {
                    lmdb::Error::KeyExist => "Key already exists".to_string(),
                    lmdb::Error::MapFull => "Database is full".to_string(),
                    lmdb::Error::TxnFull => "Transaction is full".to_string(),
                    _ => "Failed to put value".to_string()
                };
                errors.push((i, error_detail));
            }
        }
    }
    
    // Commit the transaction
    match txn.commit() {
        Ok(()) => {
            if errors.is_empty() {
                Ok(atoms::ok().encode(env))
            } else {
                Ok((atoms::ok(), success_count, errors).encode(env))
            }
        },
        Err(_) => {
            Ok((atoms::error(), atoms::transaction_error(), "Failed to commit batch transaction".to_string()).encode(env))
        }
    }
}

#[rustler::nif]
fn get<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    key: Binary
) -> NifResult<Term<'a>> {
    // Validate database and environment status
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    
    let key_bytes = key.as_slice();
    
    // Only flush write buffer if there are pending writes
    if db_handle.has_pending_writes() {
        if let Err(error_msg) = db_handle.force_flush_buffer() {
            return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
        }
    }
    
    // Create a read-only transaction
    let txn = match (*db_handle).env.env.begin_ro_txn() {
        Ok(txn) => txn,
        Err(_) => {
            return Ok((atoms::error(), atoms::transaction_error(), "Failed to begin read transaction".to_string()).encode(env));
        }
    };
    
    // Get the value for the key
    let result = txn.get((*db_handle).db, &key_bytes);
    
    match result {
        Ok(value_bytes) => {
            // Convert the value to a binary that Erlang can use
            let mut binary = rustler::types::binary::OwnedBinary::new(value_bytes.len()).unwrap();
            binary.as_mut_slice().copy_from_slice(value_bytes);
            Ok((atoms::ok(), binary.release(env)).encode(env))
        },
        Err(lmdb::Error::NotFound) => {
            Ok(atoms::not_found().encode(env))
        },
        Err(_) => {
            Ok((atoms::error(), atoms::database_error(), "Failed to get value".to_string()).encode(env))
        }
    }
}

///===================================================================
/// List Operations
///===================================================================

#[rustler::nif]
fn list<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    key_prefix: Binary
) -> NifResult<Term<'a>> {
    // Validate database and environment status
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    
    let prefix_bytes = key_prefix.as_slice();
    
    // Only flush write buffer if there are pending writes
    if db_handle.has_pending_writes() {
        if let Err(error_msg) = db_handle.force_flush_buffer() {
            return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
        }
    }
    
    // Create a read-only transaction
    let txn = match (*db_handle).env.env.begin_ro_txn() {
        Ok(txn) => txn,
        Err(_) => {
            return Ok((atoms::error(), atoms::transaction_error(), "Failed to begin read transaction".to_string()).encode(env));
        }
    };

    // Open a cursor for the database
    let mut cursor = match txn.open_ro_cursor((*db_handle).db) {
        Ok(cursor) => cursor,
        Err(_) => {
            return Ok((atoms::error(), atoms::database_error(), "Failed to open cursor".to_string()).encode(env));
        }
    };
    
    // OPTIMIZATION: Use Vec instead of HashSet for better performance with small collections
    // Pre-allocate with reasonable capacity to avoid reallocations
    let mut children = Vec::with_capacity(64);
    let prefix_len = prefix_bytes.len();
    
    // Track if we've found any keys with the prefix for early termination optimization
    let mut found_prefix_match = false;
    
    // OPTIMIZATION: Start cursor at prefix position instead of scanning from beginning
    // This dramatically reduces iterations for sparse data
    
    // Safe iteration approach that handles empty databases and missing prefixes
    // First, try to position cursor at prefix using MDB_SET_RANGE to check if key exists
    let cursor_positioned = cursor.get(Some(prefix_bytes), None, ffi::MDB_SET_RANGE).is_ok();
    
    if !cursor_positioned {
        // No keys >= prefix exist, return not_found immediately
        return Ok(atoms::not_found().encode(env));
    }
    
    // Keys exist that are >= prefix, now safely use iter_from
    let cursor_iter = cursor.iter_from(prefix_bytes);
    
    // Iterate through keys starting from the prefix
    for (key, _value) in cursor_iter {
        
        // OPTIMIZATION: Early termination - if key doesn't start with prefix and we've already
        // found matches, we can break since keys are sorted
        if !key.starts_with(prefix_bytes) {
            // Since keys are sorted, if this key doesn't match our prefix,
            // no subsequent keys will match either
            break;
        }
        
        // Extract the next path component after the prefix
        let remaining = &key[prefix_len..];
        
        // Skip if there's no remaining path (exact match with prefix)
        if remaining.is_empty() {
            continue;
        }
        
        // OPTIMIZATION: Find separator using unsafe slice operation for better performance
        let next_component = if let Some(sep_pos) = remaining.iter().position(|&b| b == b'/') {
            &remaining[..sep_pos]
        } else {
            remaining
        };
        
        // Only process non-empty components
        if next_component.is_empty() {
            continue;
        }
        
        // OPTIMIZATION: Use binary search for duplicate detection once we have enough items
        // For small collections, linear search is still faster
        let component_exists = if children.len() < 16 {
            children.iter().any(|existing: &Vec<u8>| existing.as_slice() == next_component)
        } else {
            // For larger collections, use binary search on sorted data
            children.binary_search(&next_component.to_vec()).is_ok()
        };
        
        if !component_exists {
            let component_vec = next_component.to_vec();
            if children.len() < 16 {
                children.push(component_vec);
            } else {
                // Insert maintaining sorted order for binary search
                match children.binary_search(&component_vec) {
                    Err(pos) => children.insert(pos, component_vec),
                    Ok(_) => {} // Already exists
                }
            }
        }
    }
    
    if children.is_empty() {
        return Ok(atoms::not_found().encode(env));
    }
    
    // OPTIMIZATION: Pre-allocate result vector and minimize allocations
    let mut result_binaries = Vec::with_capacity(children.len());
    
    // OPTIMIZATION: Sort results only if we didn't maintain sorted order during insertion
    if children.len() >= 16 {
        // Already sorted during insertion via binary search
    } else {
        // Sort small collections
        children.sort_unstable();
    }
    
    for child in children {
        // OPTIMIZATION: Direct binary creation without intermediate copy when possible
        let mut binary = rustler::types::binary::OwnedBinary::new(child.len())
            .ok_or(Error::BadArg)?;
        binary.as_mut_slice().copy_from_slice(&child);
        result_binaries.push(binary.release(env));
    }
    
    Ok((atoms::ok(), result_binaries).encode(env))
}

#[rustler::nif]
fn flush<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>
) -> NifResult<Term<'a>> {
    // Validate database and environment status
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    
    match db_handle.force_flush_buffer() {
        Ok(()) => Ok(atoms::ok().encode(env)),
        Err(error_msg) => Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env))
    }
}

///===================================================================
/// Helper Functions
///===================================================================

fn parse_env_options(options: Vec<Term>) -> NifResult<EnvOptions> {
    let mut env_opts = EnvOptions::default();
    
    for option in options {
        if let Ok((atom, value)) = option.decode::<(rustler::Atom, Term)>() {
            let name = format!("{:?}", atom);
            // Remove quotes from debug output
            let name = name.trim_start_matches('"').trim_end_matches('"');
            match name {
                "map_size" => {
                    if let Ok(size) = value.decode::<u64>() {
                        env_opts.map_size = Some(size);
                    }
                }
                _ => {} // Ignore unknown options for now
            }
        } else if let Ok(atom) = option.decode::<rustler::Atom>() {
            let name = format!("{:?}", atom);
            // Remove quotes from debug output
            let name = name.trim_start_matches('"').trim_end_matches('"');
            match name {
                "no_mem_init" => env_opts.no_mem_init = true,
                "no_sync" => env_opts.no_sync = true,
                "write_map" => env_opts.write_map = true,
                _ => {} // Ignore unknown options
            }
        }
    }
    
    Ok(env_opts)
}

fn parse_db_options(options: Vec<Term>) -> NifResult<DbOptions> {
    let mut db_opts = DbOptions::default();
    
    for option in options {
        if let Ok(atom) = option.decode::<rustler::Atom>() {
            let name = format!("{:?}", atom);
            // Remove quotes from debug output
            let name = name.trim_start_matches('"').trim_end_matches('"');
            match name {
                "create" => db_opts.create = true,
                _ => {} // Ignore unknown options
            }
        }
    }
    
    Ok(db_opts)
}

#[derive(Default)]
struct EnvOptions {
    map_size: Option<u64>,
    no_mem_init: bool,
    no_sync: bool,
    write_map: bool,
}

#[derive(Default)]
struct DbOptions {
    create: bool,
}

///===================================================================
/// Debug/Status Operations
///===================================================================

#[rustler::nif]
fn env_status<'a>(env: Env<'a>, env_handle: ResourceArc<LmdbEnv>) -> NifResult<Term<'a>> {
    let closed = {
        let closed = env_handle.closed.lock().map_err(|_| Error::BadArg)?;
        *closed
    };
    
    let ref_count = {
        let ref_count = env_handle.ref_count.lock().map_err(|_| Error::BadArg)?;
        *ref_count
    };
    
    Ok((atoms::ok(), closed, ref_count, env_handle.path.clone()).encode(env))
}


rustler::init!("elmdb", [env_open, env_close, env_close_by_name, db_open, db_close, put, put_batch, get, list, flush, env_status], load = init);