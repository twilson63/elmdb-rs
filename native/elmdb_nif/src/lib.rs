use rustler::{Env, Term, NifResult, Error, Encoder, ResourceArc};
use rustler::types::binary::Binary;
use std::collections::{HashMap, HashSet};
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

// Resource types for LMDB handles
#[derive(Debug)]
pub struct LmdbEnv {
    env: Environment,
    path: String,
}

pub struct LmdbDatabase {
    db: Database,
    env: ResourceArc<LmdbEnv>,
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
    
    // Remove from global environments map
    // This will drop the ResourceArc, which will trigger the Drop impl
    {
        let mut environments = ENVIRONMENTS.lock().unwrap();
        if environments.remove(path_str).is_some() {
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
    
    // Create database resource
    let lmdb_db = LmdbDatabase { 
        db: database,
        env: env_handle.clone(),
    };
    let resource = ResourceArc::new(lmdb_db);
    
    Ok((atoms::ok(), resource).encode(env))
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
    let key_bytes = key.as_slice();
    let value_bytes = value.as_slice();
    
    // Create a write transaction
    let mut txn = match (*db_handle).env.env.begin_rw_txn() {
        Ok(txn) => txn,
        Err(_) => {
            return Ok((atoms::error(), atoms::transaction_error(), "Failed to begin write transaction".to_string()).encode(env));
        }
    };
    
    // Put the key-value pair
    let result = txn.put((*db_handle).db, &key_bytes, &value_bytes, WriteFlags::empty());
    
    match result {
        Ok(()) => {
            // Commit the transaction
            match txn.commit() {
                Ok(()) => Ok(atoms::ok().encode(env)),
                Err(_) => Ok((atoms::error(), atoms::transaction_error(), "Failed to commit transaction".to_string()).encode(env))
            }
        },
        Err(lmdb_err) => {
            // Transaction will be automatically aborted when dropped
            match lmdb_err {
                lmdb::Error::KeyExist => Ok((atoms::error(), atoms::key_exist(), "Key already exists".to_string()).encode(env)),
                lmdb::Error::MapFull => Ok((atoms::error(), atoms::map_full(), "Database is full".to_string()).encode(env)),
                lmdb::Error::TxnFull => Ok((atoms::error(), atoms::txn_full(), "Transaction is full".to_string()).encode(env)),
                _ => Ok((atoms::error(), atoms::database_error(), "Failed to put value".to_string()).encode(env))
            }
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
}

#[derive(Default)]
struct DbOptions {
    create: bool,
}

rustler::init!("elmdb", [env_open, env_close, env_close_by_name, db_open, put, get, list], load = init);