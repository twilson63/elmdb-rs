use rustler::{Env, Term, NifResult, Error, Encoder, ResourceArc};
use rustler::types::binary::Binary;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex};
use std::path::Path;
use lmdb::{Environment, EnvironmentFlags, Database, DatabaseFlags, Transaction, WriteFlags, Cursor};

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
    }
}

// Write buffer for batching operations
#[derive(Debug, Clone)]
struct WriteOperation {
    key: Vec<u8>,
    value: Vec<u8>,
}

#[derive(Debug)]
struct WriteBuffer {
    operations: VecDeque<WriteOperation>,
    max_size: usize,
}

impl WriteBuffer {
    fn new(max_size: usize) -> Self {
        Self {
            operations: VecDeque::new(),
            max_size,
        }
    }

    fn add(&mut self, key: Vec<u8>, value: Vec<u8>) -> bool {
        self.operations.push_back(WriteOperation { key, value });
        self.operations.len() >= self.max_size
    }

    fn is_empty(&self) -> bool {
        self.operations.is_empty()
    }

    fn len(&self) -> usize {
        self.operations.len()
    }

    fn drain(&mut self) -> Vec<WriteOperation> {
        self.operations.drain(..).collect()
    }
}

// Resource types for LMDB handles
#[derive(Debug)]
pub struct LmdbEnv {
    env: Environment,
    path: String,
    closed: Arc<Mutex<bool>>,
}

pub struct LmdbDatabase {
    db: Database,
    env: ResourceArc<LmdbEnv>,
    write_buffer: Arc<Mutex<WriteBuffer>>,
}

// Global state for managing environments (singleton pattern)
lazy_static::lazy_static! {
    static ref ENVIRONMENTS: Arc<Mutex<HashMap<String, ResourceArc<LmdbEnv>>>> = 
        Arc::new(Mutex::new(HashMap::new()));
}

// Initialize NIF
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
        Err(_) => {
            return Ok((atoms::error(), atoms::environment_error()).encode(env));
        }
    };
    
    // Create our wrapper struct
    let lmdb_env = LmdbEnv { 
        env: lmdb_environment,
        path: path_str.to_string(),
        closed: Arc::new(Mutex::new(false)),
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
        env_handle.env.create_db(None, flags)
    } else {
        env_handle.env.open_db(None)
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
    
    // Create database resource with write buffer (default buffer size: 1000 operations)
    let lmdb_db = LmdbDatabase { 
        db: database,
        env: env_handle.clone(),
        write_buffer: Arc::new(Mutex::new(WriteBuffer::new(1000))),
    };
    let resource = ResourceArc::new(lmdb_db);
    
    Ok((atoms::ok(), resource).encode(env))
}

///===================================================================
/// Write Buffer Management
///===================================================================

impl LmdbDatabase {
    fn flush_write_buffer(&self) -> Result<(), String> {
        let mut buffer = self.write_buffer.lock().map_err(|_| "Failed to lock write buffer")?;
        
        if buffer.is_empty() {
            return Ok(());
        }
        
        let operations = buffer.drain();
        drop(buffer); // Release lock before starting transaction
        
        // Create a write transaction for the batch
        let mut txn = self.env.env.begin_rw_txn()
            .map_err(|_| "Failed to begin write transaction")?;
        
        // Execute all buffered operations
        for op in operations {
            txn.put(self.db, &op.key, &op.value, WriteFlags::empty())
                .map_err(|e| format!("Failed to put value: {:?}", e))?;
        }
        
        // Commit the transaction
        txn.commit()
            .map_err(|_| "Failed to commit batch transaction")?;
        
        Ok(())
    }

    fn validate_database(&self) -> Result<(), String> {
        // Check if environment is marked as closed
        {
            let closed = self.env.closed.lock().map_err(|_| "Failed to check environment status")?;
            if *closed {
                return Err("Database environment is closed".to_string());
            }
        }
        
        // Quick validation by attempting to begin a read transaction
        let _txn = self.env.env.begin_ro_txn()
            .map_err(|_| "Database environment is invalid or closed")?;
        Ok(())
    }
}

impl Drop for LmdbDatabase {
    fn drop(&mut self) {
        // Attempt to flush any remaining buffered writes when the database is dropped
        // We ignore errors here since we can't handle them in Drop
        let _ = self.flush_write_buffer();
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
    // Validate database is still open before proceeding
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }

    let key_vec = key.as_slice().to_vec();
    let value_vec = value.as_slice().to_vec();
    
    // Add to write buffer
    let needs_flush = {
        let mut buffer = match db_handle.write_buffer.lock() {
            Ok(buffer) => buffer,
            Err(_) => {
                return Ok((atoms::error(), atoms::transaction_error(), "Failed to lock write buffer".to_string()).encode(env));
            }
        };
        buffer.add(key_vec, value_vec)
    };
    
    // Flush if buffer is full
    if needs_flush {
        if let Err(error_msg) = db_handle.flush_write_buffer() {
            return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
        }
    }
    
    Ok(atoms::ok().encode(env))
}

#[rustler::nif]
fn put_batch<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    key_value_pairs: Vec<(Binary, Binary)>
) -> NifResult<Term<'a>> {
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
    let key_bytes = key.as_slice();
    
    // Flush write buffer to ensure consistency
    if let Err(error_msg) = db_handle.flush_write_buffer() {
        return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
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
    let prefix_bytes = key_prefix.as_slice();
    
    // Flush write buffer to ensure consistency
    if let Err(error_msg) = db_handle.flush_write_buffer() {
        return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
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
    
    // Start from the first key and iterate through all keys
    let mut children = HashSet::new();
    
    // Iterate through all keys using cursor
    let mut cursor_iter = cursor.iter_start();
    while let Some((key, _value)) = cursor_iter.next() {
        // Check if the key starts with our prefix
        if key.starts_with(prefix_bytes) {
            // Extract the next path component after the prefix
            let remaining = &key[prefix_bytes.len()..];
            
            // Skip if there's no remaining path (exact match with prefix)
            if !remaining.is_empty() {
                // Find the next separator (assuming '/' as path separator)
                let next_component = if let Some(sep_pos) = remaining.iter().position(|&b| b == b'/') {
                    &remaining[..sep_pos]
                } else {
                    remaining
                };
                
                // Only add non-empty components
                if !next_component.is_empty() {
                    children.insert(next_component.to_vec());
                }
            }
        } else if !children.is_empty() {
            // If we've found children and now hit a key that doesn't match prefix, 
            // we can break since keys are sorted
            break;
        }
    }
    
    // Convert the set to a vector of binaries
    let mut result_binaries = Vec::new();
    for child in children {
        let mut binary = rustler::types::binary::OwnedBinary::new(child.len()).unwrap();
        binary.as_mut_slice().copy_from_slice(&child);
        result_binaries.push(binary.release(env));
    }
    
    if result_binaries.is_empty() {
        Ok(atoms::not_found().encode(env))
    } else {
        Ok((atoms::ok(), result_binaries).encode(env))
    }
}

#[rustler::nif]
fn flush<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>
) -> NifResult<Term<'a>> {
    match db_handle.flush_write_buffer() {
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

rustler::init!("elmdb", [env_open, env_close, env_close_by_name, db_open, put, put_batch, get, list, flush], load = init);