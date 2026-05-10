//! elmdb_nif - High-performance LMDB bindings for Erlang via Rust NIF
//!
//! This module implements a Native Implemented Function (NIF) that provides
//! Erlang/Elixir applications with access to LMDB (Lightning Memory-Mapped Database).
//!
//! # Architecture
//!
//! The implementation uses a two-layer architecture:
//! - **Resource Management**: Environments and databases are managed as Erlang resources
//! - **Write Optimization**: Dual-map overlay with background flush worker
//!
//! # Safety
//!
//! - All LMDB operations are wrapped in safe Rust abstractions
//! - Resources are automatically cleaned up when no longer referenced
//! - Thread-safe through Arc/ArcSwap/Mutex wrappers
//! - Prevents use-after-close errors through validation checks
//!
//! # Performance
//!
//! Key optimizations include:
//! - Lock-free put via scc::HashMap behind ArcSwap
//! - Background flush worker decouples LMDB I/O from Erlang schedulers
//! - Zero-copy reads through memory mapping
//! - Efficient cursor iteration for list operations
//! - Early termination for prefix searches

use rustler::{Env, Term, NifResult, Error, Encoder, ResourceArc};
use rustler::types::binary::Binary;
use rustler::types::binary::OwnedBinary;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::sync::mpsc::{self, Sender, Receiver};
use std::thread::{self, JoinHandle};
use arc_swap::ArcSwap;
use std::path::Path;
use scc::HashMap as SccHashMap;
use lmdb::{Environment, EnvironmentFlags, Database, DatabaseFlags, Transaction, WriteFlags, Cursor};

// LMDB cursor operation constants (instead of importing lmdb-sys only for constants from lmdb_sys::ffi).
// To be improved in the future.
const MDB_FIRST: u32 = 0;
const MDB_NEXT: u32 = 8;
const MDB_SET_RANGE: u32 = 17;
// Default LMDB max key size. This is controlled by LMDB's compile-time MDB_MAXKEYSIZE.
// If the Rust lmdb crate exposes mdb_env_get_maxkeysize safely in the future, prefer that.
const LMDB_DEFAULT_MAX_KEY_SIZE: usize = 511;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        not_found,
        nif_not_loaded,
        // LMDB-specific atoms
        map_size,
        max_readers,
        no_mem_init,
        no_sync,
        write_map,
        no_readahead,
        create,
        iterator,
        start,
        undefined,
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
        page_not_found,
        panic,
        invalid,
        dbs_full,
        readers_full,
        tls_full,
        cursor_full,
        page_full,
        // Specific environment errors
        directory_not_found,
        no_space,
        io_error,
        corrupted,
        version_mismatch,
        map_resized,
        incompatible,
        bad_rslot,
        bad_txn,
        bad_val_size,
        bad_dbi,
        validation_error,
    }
}

/// Iterator cursor token passed between Erlang and Rust.
///
/// The token is intentionally stateless on the Rust side:
/// - `{iterator, start}` means "start before the first key"
/// - `{iterator, LastKey}` means "start after LastKey"
enum IteratorCursor {
    Start,
    AfterKey(Vec<u8>),
}

struct DbState {
    cached_db: Option<(Database, u64)>,
    create_if_missing: bool,
    closed: bool,
}

type OverlayMap = SccHashMap<Vec<u8>, Vec<u8>, ahash::RandomState>;
type FlushSyncState = Arc<(Mutex<Option<Result<(), String>>>, Condvar)>;

enum WorkerCommand {
    Flush,
    FlushSync(FlushSyncState),
    Shutdown,
}

/// LMDB Environment resource
///
/// Represents an LMDB environment that can contain multiple databases.
/// Environments are reference-counted and shared across database instances.
#[derive(Debug)]
pub struct LmdbEnv {
    /// Path to the database directory
    path: String,
    /// Environment options used when reopening
    options: Arc<std::sync::RwLock<EnvOptions>>,
    /// Mutable runtime environment state
    state: Arc<std::sync::RwLock<EnvState>>,
    /// Reference count for active databases using this environment
    ref_count: Arc<Mutex<usize>>,
    /// Atomic generation counter for lock-free fast path
    generation: AtomicU64,
}

/// LMDB Database resource
///
/// Represents a database within an LMDB environment.
/// Uses a dual-map overlay for lock-free writes with a background flush worker.
pub struct LmdbDatabase {
    env: ResourceArc<LmdbEnv>,
    /// Active overlay map: all new puts land here
    active: ArcSwap<OverlayMap>,
    /// Draining map: set during flush, readable for get consistency
    draining: ArcSwap<Option<Arc<OverlayMap>>>,
    /// Write operation counter for flush threshold
    op_count: AtomicUsize,
    /// Flush threshold (number of ops before triggering background flush)
    batch_size: AtomicUsize,
    /// Coalesces redundant flush signals to worker
    flush_pending: AtomicBool,
    /// Cache: true when db is closed (atomic fast-path, no lock needed)
    is_closed: AtomicBool,
    /// Atomic fast-path: true when fatal_error is set
    has_fatal_error: AtomicBool,
    /// Metadata state (cached_db, closed, create_if_missing)
    state: Mutex<DbState>,
    /// Fatal flush error set by worker thread
    fatal_error: Mutex<Option<String>>,
    /// Lock-free read fast path: cached (Arc<Environment>, Database, generation)
    hot_handles: ArcSwap<Option<(Arc<Environment>, Database, u64)>>,
    /// Channel to send commands to the worker thread
    worker_tx: Mutex<Option<Sender<WorkerCommand>>>,
    /// Handle to the worker thread for joining
    worker_handle: Mutex<Option<JoinHandle<()>>>,
}

#[derive(Debug)]
struct EnvState {
    env: Option<Arc<Environment>>,
    close_requested: bool,
    generation: u64,
}

// Global registry of open environments
//
// Ensures that each directory path has at most one environment open,
// preventing LMDB conflicts and improving resource sharing.
lazy_static::lazy_static! {
    static ref ENVIRONMENTS: Arc<Mutex<HashMap<String, ResourceArc<LmdbEnv>>>> =
        Arc::new(Mutex::new(HashMap::new()));
    static ref DATABASES: Arc<Mutex<HashMap<String, ResourceArc<LmdbDatabase>>>> =
        Arc::new(Mutex::new(HashMap::new()));
}

/// Initialize the NIF module
///
/// Registers resource types with the Erlang runtime.
/// This function is called automatically when the NIF is loaded.
fn init(env: Env, _info: Term) -> bool {
    rustler::resource!(LmdbEnv, env) && rustler::resource!(LmdbDatabase, env)
}

fn new_overlay_map() -> OverlayMap {
    SccHashMap::with_hasher(ahash::RandomState::default())
}

fn build_environment(path: &str, options: &EnvOptions) -> Result<Environment, lmdb::Error> {
    let mut env_builder = Environment::new();

    if let Some(map_size) = options.map_size {
        env_builder.set_map_size(map_size as usize);
    } else {
        env_builder.set_map_size(1024 * 1024 * 1024);
    }

    if let Some(max_readers) = options.max_readers {
        env_builder.set_max_readers(max_readers);
    }

    let mut flags = EnvironmentFlags::empty();
    if options.no_mem_init {
        flags |= EnvironmentFlags::NO_MEM_INIT;
    }
    if options.no_sync {
        flags |= EnvironmentFlags::NO_SYNC;
    }
    if options.no_lock {
        flags |= EnvironmentFlags::NO_LOCK;
    }
    if options.write_map {
        flags |= EnvironmentFlags::WRITE_MAP;
    }
    if options.no_readahead {
        flags |= EnvironmentFlags::NO_READAHEAD;
    }
    env_builder.set_flags(flags);

    env_builder.open(Path::new(path))
}

impl LmdbEnv {
    fn set_options(&self, options: EnvOptions) -> Result<(), String> {
        let mut stored = self
            .options
            .write()
            .map_err(|_| "Failed to write environment options".to_string())?;
        *stored = options;
        Ok(())
    }

    fn write_buffer_size(&self) -> Result<usize, String> {
        let options = self
            .options
            .read()
            .map_err(|_| "Failed to read environment options".to_string())?;
        Ok(options.batch_size.unwrap_or(1000))
    }

    fn ensure_open(&self) -> Result<(Arc<Environment>, u64), String> {
        {
            let state = self
                .state
                .read()
                .map_err(|_| "Failed to read environment state".to_string())?;

            if let Some(existing_env) = state.env.as_ref() {
                if !state.close_requested {
                    return Ok((existing_env.clone(), state.generation));
                }
                if Arc::strong_count(existing_env) > 1 {
                    return Ok((existing_env.clone(), state.generation));
                }
            }
        }

        let mut state = self
            .state
            .write()
            .map_err(|_| "Failed to write environment state".to_string())?;

        if let Some(existing_env) = state.env.as_ref() {
            if !state.close_requested {
                return Ok((existing_env.clone(), state.generation));
            }
            if Arc::strong_count(existing_env) > 1 {
                return Ok((existing_env.clone(), state.generation));
            }
            state.env = None;
            state.close_requested = false;
            state.generation += 1;
            self.generation.fetch_add(1, Ordering::Release);
        }

        let options = self
            .options
            .read()
            .map_err(|_| "Failed to read environment options".to_string())?
            .clone();

        let reopened = build_environment(&self.path, &options)
            .map_err(|e| format!("Failed to open environment: {:?}", e))?;
        let reopened = Arc::new(reopened);
        state.env = Some(reopened.clone());
        state.close_requested = false;
        state.generation += 1;
        Ok((reopened, state.generation))
    }

    fn request_close(&self) -> Result<(), String> {
        let mut state = self
            .state
            .write()
            .map_err(|_| "Failed to write environment state".to_string())?;

        state.close_requested = true;
        if let Some(existing_env) = state.env.as_ref() {
            if Arc::strong_count(existing_env) == 1 {
                state.env = None;
                state.close_requested = false;
                state.generation += 1;
                self.generation.fetch_add(1, Ordering::Release);
            }
        }

        Ok(())
    }

    fn is_closed(&self) -> Result<bool, String> {
        let state = self
            .state
            .read()
            .map_err(|_| "Failed to read environment state".to_string())?;
        Ok(state.env.is_none() || state.close_requested)
    }
}

fn do_flush(db: &LmdbDatabase) -> Result<(), String> {
    let new_map = Arc::new(new_overlay_map());
    let old_map = db.active.load_full();
    db.draining.store(Arc::new(Some(old_map.clone())));
    db.active.store(new_map);
    let _ = db.op_count.swap(0, Ordering::AcqRel);

    if old_map.is_empty() {
        db.draining.store(Arc::new(None));
        return Ok(());
    }

    let (live_env, live_db) = match db.fast_get_handles() {
        Ok(handles) => handles,
        Err(e) => {
            restore_failed_flush(db, old_map);
            return Err(e);
        }
    };

    let mut txn = match live_env.begin_rw_txn() {
        Ok(txn) => txn,
        Err(_) => {
            restore_failed_flush(db, old_map);
            return Err("Failed to begin write transaction".to_string());
        }
    };

    let mut write_err = None;
    (*old_map).iter_sync(|k, v| {
        if let Err(e) = txn.put(live_db, k, v, WriteFlags::empty()) {
            write_err = Some(format!("Failed to put value: {:?}", e));
            return false;
        }
        true
    });

    if let Some(e) = write_err {
        drop(txn);
        restore_failed_flush(db, old_map);
        return Err(e);
    }

    match txn.commit() {
        Ok(()) => {
            db.draining.store(Arc::new(None));
            Ok(())
        }
        Err(e) => {
            restore_failed_flush(db, old_map);
            Err(format!("Failed to commit batch transaction: {}", e))
        }
    }
}

fn restore_failed_flush(db: &LmdbDatabase, failed_map: Arc<OverlayMap>) {
    let current = db.active.load();
    let mut count = 0usize;
    (*failed_map).iter_sync(|k, v| {
        let _ = current.insert_sync(k.clone(), v.clone());
        count += 1;
        true
    });
    db.op_count.fetch_add(count, Ordering::Relaxed);
    db.draining.store(Arc::new(None));
}

fn drain_remaining_sync_waiters(rx: &Receiver<WorkerCommand>, fatal_error: Option<String>) {
    while let Ok(cmd) = rx.try_recv() {
        if let WorkerCommand::FlushSync(signal) = cmd {
            let err_msg = fatal_error.clone().unwrap_or_else(|| "Worker shut down".to_string());
            let (lock, cvar) = &*signal;
            if let Ok(mut guard) = lock.lock() {
                *guard = Some(Err(err_msg));
            }
            cvar.notify_all();
        }
    }
}

fn worker_loop(rx: Receiver<WorkerCommand>, db: ResourceArc<LmdbDatabase>) {
    let mut exit_error: Option<String> = None;
    loop {
        match rx.recv() {
            Ok(WorkerCommand::Flush) => {
                db.flush_pending.store(false, Ordering::Release);
                let result = do_flush(&db);
                if let Err(e) = result {
                    if let Ok(mut guard) = db.fatal_error.lock() {
                        *guard = Some(e.clone());
                    }
                    db.has_fatal_error.store(true, Ordering::Release);
                    exit_error = Some(e);
                    break;
                }
            }
            Ok(WorkerCommand::FlushSync(signal)) => {
                db.flush_pending.store(false, Ordering::Release);
                let result = do_flush(&db);
                let is_err = result.is_err();
                if is_err {
                    let e = result.as_ref().unwrap_err().clone();
                    if let Ok(mut guard) = db.fatal_error.lock() {
                        *guard = Some(e.clone());
                    }
                    db.has_fatal_error.store(true, Ordering::Release);
                    exit_error = Some(e);
                }
                {
                    let (lock, cvar) = &*signal;
                    if let Ok(mut guard) = lock.lock() {
                        *guard = Some(result);
                    }
                    cvar.notify_all();
                }
                if is_err {
                    break;
                }
            }
            Ok(WorkerCommand::Shutdown) => {
                let result = do_flush(&db);
                if let Err(e) = result {
                    if let Ok(mut guard) = db.fatal_error.lock() {
                        *guard = Some(e.clone());
                    }
                    db.has_fatal_error.store(true, Ordering::Release);
                    exit_error = Some(e);
                }
                break;
            }
            Err(_) => break,
        }
    }
    drain_remaining_sync_waiters(&rx, exit_error);
    db.flush_pending.store(false, Ordering::Release);
}

fn flush_sync(db_handle: &LmdbDatabase) -> Result<(), String> {
    let signal = Arc::new((Mutex::new(None::<Result<(), String>>), Condvar::new()));
    {
        let tx = db_handle
            .worker_tx
            .lock()
            .map_err(|_| "Failed to lock worker channel".to_string())?;
        if let Some(ref sender) = *tx {
            sender
                .send(WorkerCommand::FlushSync(signal.clone()))
                .map_err(|_| "Worker thread is gone".to_string())?;
        } else {
            return Err("Worker thread is not running".to_string());
        }
    }
    let (lock, cvar) = &*signal;
    let mut result = lock
        .lock()
        .map_err(|_| "Failed to lock signal".to_string())?;
    while result.is_none() {
        result = cvar
            .wait(result)
            .map_err(|_| "Condvar wait failed".to_string())?;
    }
    result.take().unwrap()
}

fn spawn_worker(resource: &ResourceArc<LmdbDatabase>) -> (Sender<WorkerCommand>, JoinHandle<()>) {
    let (tx, rx) = mpsc::channel();
    let worker_resource = resource.clone();
    let handle = thread::spawn(move || {
        worker_loop(rx, worker_resource);
    });
    (tx, handle)
}

fn ensure_worker(db_handle: &ResourceArc<LmdbDatabase>) {
    let needs_worker = db_handle
        .worker_tx
        .lock()
        .map(|g| g.is_none())
        .unwrap_or(false);
    if needs_worker {
        let (tx, handle) = spawn_worker(db_handle);
        if let Ok(mut wtx) = db_handle.worker_tx.lock() {
            if wtx.is_none() {
                *wtx = Some(tx);
                if let Ok(mut wh) = db_handle.worker_handle.lock() {
                    *wh = Some(handle);
                }
            }
        }
    }
}

fn soft_close_db(db_handle: &ResourceArc<LmdbDatabase>) -> Result<(), String> {
    db_handle.is_closed.store(true, Ordering::Release);

    {
        let mut tx = db_handle
            .worker_tx
            .lock()
            .map_err(|_| "Failed to lock worker channel".to_string())?;
        if let Some(sender) = tx.take() {
            let _ = sender.send(WorkerCommand::Shutdown);
        }
    }
    {
        let mut wh = db_handle
            .worker_handle
            .lock()
            .map_err(|_| "Failed to lock worker handle".to_string())?;
        if let Some(handle) = wh.take() {
            let _ = handle.join();
        }
    }

    let fatal_err = db_handle
        .fatal_error
        .lock()
        .ok()
        .and_then(|g| g.clone());

    let was_open = {
        let mut state = db_handle
            .state
            .lock()
            .map_err(|_| "Failed to lock database state".to_string())?;
        let was_open = !state.closed;
        state.closed = true;
        state.cached_db = None;
        was_open
    };

    db_handle.hot_handles.store(Arc::new(None));

    if was_open {
        let mut ref_count = db_handle
            .env
            .ref_count
            .lock()
            .map_err(|_| "Failed to update environment reference count".to_string())?;
        if *ref_count > 0 {
            *ref_count -= 1;
        }
    }

    if let Some(err) = fatal_err {
        return Err(err);
    }

    Ok(())
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
        std::str::from_utf8(&chars).map_err(|_| Error::BadArg)?.to_string()
    } else {
        return Err(Error::BadArg);
    };
    let path_str = &path_string;
    let has_options = !options.is_empty();
    let parsed_options = parse_env_options(options)?;

    if let Some(existing_env) = {
        let environments = ENVIRONMENTS.lock().unwrap();
        environments.get(path_str).cloned()
    } {
        if !Path::new(path_str).exists() {
            if let Some(db_handle) = {
                let databases = DATABASES.lock().unwrap();
                databases.get(path_str).cloned()
            } {
                db_handle.hot_handles.store(Arc::new(None));
            }
            if let Err(error_msg) = existing_env.request_close() {
                return Ok((atoms::error(), atoms::environment_error(), error_msg).encode(env));
            }
        }
        if has_options {
            if let Err(error_msg) = existing_env.set_options(parsed_options) {
                return Ok((atoms::error(), atoms::environment_error(), error_msg).encode(env));
            }
        }
        if let Err(error_msg) = existing_env.ensure_open() {
            return Ok((atoms::error(), atoms::environment_error(), error_msg).encode(env));
        }
        return Ok((atoms::ok(), existing_env).encode(env));
    }

    let lmdb_env_result = build_environment(path_str, &parsed_options);

    let lmdb_environment = match lmdb_env_result {
        Ok(env) => env,
        Err(e) => {
            let path = Path::new(path_str);

            let error_atom = if !path.exists() {
                atoms::directory_not_found()
            } else if path.is_file() {
                atoms::invalid_path()
            } else {
                match std::fs::File::create(path.join(".lmdb_test")) {
                    Ok(_) => {
                        let _ = std::fs::remove_file(path.join(".lmdb_test"));
                        lmdb_error_to_atom(e)
                    }
                    Err(io_err) => match io_err.kind() {
                        std::io::ErrorKind::PermissionDenied => atoms::permission_denied(),
                        _ => atoms::environment_error(),
                    },
                }
            };

            return Ok((atoms::error(), error_atom).encode(env));
        }
    };

    let lmdb_env = LmdbEnv {
        path: path_str.to_string(),
        options: Arc::new(std::sync::RwLock::new(parsed_options.clone())),
        state: Arc::new(std::sync::RwLock::new(EnvState {
            env: Some(Arc::new(lmdb_environment)),
            close_requested: false,
            generation: 1,
        })),
        ref_count: Arc::new(Mutex::new(0)),
        generation: AtomicU64::new(1),
    };
    let resource = ResourceArc::new(lmdb_env);

    {
        let mut environments = ENVIRONMENTS.lock().unwrap();
        environments.insert(path_str.to_string(), resource.clone());
    }

    Ok((atoms::ok(), resource).encode(env))
}

#[rustler::nif]
fn env_sync<'a>(env: Env<'a>, env_handle: ResourceArc<LmdbEnv>) -> NifResult<Term<'a>> {
    let (live_env, _) = match env_handle.ensure_open() {
        Ok(data) => data,
        Err(error_msg) => {
            return Ok((atoms::error(), atoms::environment_error(), error_msg).encode(env));
        }
    };

    match live_env.sync(true) {
        Ok(()) => Ok(atoms::ok().encode(env)),
        Err(err_msg) => Ok(
            (atoms::error(), atoms::environment_error(), format!("Environment sync failed: {}", err_msg))
                .encode(env),
        ),
    }
}

#[rustler::nif]
fn env_close<'a>(env: Env<'a>, env_handle: ResourceArc<LmdbEnv>) -> NifResult<Term<'a>> {
    if let Some(db_handle) = {
        let databases = DATABASES.lock().unwrap();
        databases.get(&env_handle.path).cloned()
    } {
        let _ = soft_close_db(&db_handle);
    }

    if let Err(error_msg) = env_handle.request_close() {
        return Ok((atoms::error(), atoms::environment_error(), error_msg).encode(env));
    }

    Ok(atoms::ok().encode(env))
}

#[rustler::nif]
fn env_close_by_name<'a>(env: Env<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    let path_string = if let Ok(binary) = path.decode::<Binary>() {
        std::str::from_utf8(&binary).map_err(|_| Error::BadArg)?.to_string()
    } else if let Ok(string) = path.decode::<String>() {
        string
    } else if let Ok(chars) = path.decode::<Vec<u8>>() {
        std::str::from_utf8(&chars).map_err(|_| Error::BadArg)?.to_string()
    } else {
        return Err(Error::BadArg);
    };
    let path_str = &path_string;

    let env_handle = {
        let environments = ENVIRONMENTS.lock().unwrap();
        environments.get(path_str).cloned()
    };

    if let Some(db_handle) = {
        let databases = DATABASES.lock().unwrap();
        databases.get(path_str).cloned()
    } {
        let _ = soft_close_db(&db_handle);
    }

    if let Some(env_handle) = env_handle {
        if let Err(error_msg) = env_handle.request_close() {
            return Ok((atoms::error(), atoms::environment_error(), error_msg).encode(env));
        }
        Ok(atoms::ok().encode(env))
    } else {
        Ok((atoms::error(), atoms::not_found()).encode(env))
    }
}

///===================================================================
/// Database Operations
///===================================================================

#[rustler::nif]
fn db_close<'a>(env: Env<'a>, db_handle: ResourceArc<LmdbDatabase>) -> NifResult<Term<'a>> {
    match soft_close_db(&db_handle) {
        Ok(()) => Ok(atoms::ok().encode(env)),
        Err(error_msg) => Ok((atoms::error(), atoms::database_error(), error_msg).encode(env)),
    }
}

#[rustler::nif]
fn db_open<'a>(
    env: Env<'a>,
    env_handle: ResourceArc<LmdbEnv>,
    options: Vec<Term<'a>>,
) -> NifResult<Term<'a>> {
    let parsed_options = parse_db_options(options)?;
    if let Err(error_msg) = env_handle.ensure_open() {
        return Ok((atoms::error(), atoms::environment_error(), error_msg).encode(env));
    }
    let batch_size = match env_handle.write_buffer_size() {
        Ok(size) => size,
        Err(error_msg) => {
            return Ok((atoms::error(), atoms::environment_error(), error_msg).encode(env));
        }
    };

    let db_key = env_handle.path.clone();
    if let Some(existing_db) = {
        let databases = DATABASES.lock().map_err(|_| Error::BadArg)?;
        databases.get(&db_key).cloned()
    } {
        if parsed_options.create {
            if let Err(error_msg) = existing_db.set_create_if_missing(true) {
                return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
            }
        }
        existing_db.batch_size.store(batch_size, Ordering::Release);
        if existing_db.has_fatal_error.load(Ordering::Acquire) {
            if let Ok(mut wh) = existing_db.worker_handle.lock() {
                if let Some(handle) = wh.take() {
                    let _ = handle.join();
                }
            }
            let (new_tx, new_handle) = spawn_worker(&existing_db);
            if let Ok(mut tx) = existing_db.worker_tx.lock() {
                let _ = tx.replace(new_tx);
            }
            if let Ok(mut wh) = existing_db.worker_handle.lock() {
                *wh = Some(new_handle);
            }
            if let Ok(mut fe) = existing_db.fatal_error.lock() {
                *fe = None;
            }
            existing_db.has_fatal_error.store(false, Ordering::Release);
            existing_db.flush_pending.store(false, Ordering::Release);
        }
        if let Err(error_msg) = existing_db.reopen_if_closed() {
            return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
        }
        ensure_worker(&existing_db);
        if let Err(error_msg) = existing_db.validate_database() {
            return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
        }
        return Ok((atoms::ok(), existing_db).encode(env));
    }

    // Increment reference count for this environment
    {
        let mut ref_count = env_handle.ref_count.lock().map_err(|_| Error::BadArg)?;
        *ref_count += 1;
    }

    let lmdb_db = LmdbDatabase {
        env: env_handle.clone(),
        active: ArcSwap::from_pointee(new_overlay_map()),
        draining: ArcSwap::from_pointee(None),
        op_count: AtomicUsize::new(0),
        batch_size: AtomicUsize::new(batch_size),
        flush_pending: AtomicBool::new(false),
        state: Mutex::new(DbState {
            cached_db: None,
            create_if_missing: parsed_options.create,
            closed: false,
        }),
        fatal_error: Mutex::new(None),
        has_fatal_error: AtomicBool::new(false),
        is_closed: AtomicBool::new(false),
        hot_handles: ArcSwap::from_pointee(None),
        worker_tx: Mutex::new(None),
        worker_handle: Mutex::new(None),
    };
    let resource = ResourceArc::new(lmdb_db);

    let (tx, handle) = spawn_worker(&resource);
    if let Ok(mut wtx) = resource.worker_tx.lock() {
        *wtx = Some(tx);
    }
    if let Ok(mut wh) = resource.worker_handle.lock() {
        *wh = Some(handle);
    }

    {
        let mut databases = DATABASES.lock().map_err(|_| Error::BadArg)?;
        databases.insert(db_key, resource.clone());
    }

    if let Err(error_msg) = resource.validate_database() {
        {
            let mut databases = DATABASES.lock().map_err(|_| Error::BadArg)?;
            databases.remove(&env_handle.path);
        }
        {
            let mut ref_count = env_handle.ref_count.lock().map_err(|_| Error::BadArg)?;
            if *ref_count > 0 {
                *ref_count -= 1;
            }
        }
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }

    Ok((atoms::ok(), resource).encode(env))
}

impl LmdbDatabase {
    fn set_create_if_missing(&self, create: bool) -> Result<(), String> {
        if !create {
            return Ok(());
        }
        let mut state = self
            .state
            .lock()
            .map_err(|_| "Failed to lock database state".to_string())?;
        state.create_if_missing = true;
        Ok(())
    }

    fn reopen_if_closed(&self) -> Result<(), String> {
        let reopened = {
            let mut state = self
                .state
                .lock()
                .map_err(|_| "Failed to lock database state")?;
            if state.closed {
                state.closed = false;
                self.is_closed.store(false, Ordering::Release);
                true
            } else {
                false
            }
        };

        if reopened {
            let mut ref_count = self
                .env
                .ref_count
                .lock()
                .map_err(|_| "Failed to update environment reference count")?;
            *ref_count += 1;

            if let Ok(mut fe) = self.fatal_error.lock() {
                *fe = None;
            }
            self.has_fatal_error.store(false, Ordering::Release);
        }
        Ok(())
    }

    fn ensure_open_handles(&self) -> Result<(Arc<Environment>, Database), String> {
        let (live_env, env_generation) = self.env.ensure_open()?;
        self.reopen_if_closed()?;

        let mut state = self
            .state
            .lock()
            .map_err(|_| "Failed to lock database state".to_string())?;

        if let Some((cached_db, cached_generation)) = state.cached_db {
            if cached_generation == env_generation {
                return Ok((live_env, cached_db));
            }
        }

        let db = if state.create_if_missing {
            live_env.create_db(None, DatabaseFlags::empty())
        } else {
            match live_env.create_db(None, DatabaseFlags::empty()) {
                Ok(db) => Ok(db),
                Err(_) => live_env.open_db(None),
            }
        }
        .map_err(|e| format!("Failed to open database: {:?}", e))?;

        state.cached_db = Some((db, env_generation));

        let gen = self.env.generation.load(Ordering::Acquire);
        self.hot_handles
            .store(Arc::new(Some((live_env.clone(), db, gen))));

        Ok((live_env, db))
    }

    fn fast_get_handles(&self) -> Result<(Arc<Environment>, Database), String> {
        if !self.is_closed.load(Ordering::Relaxed) {
            let current_gen = self.env.generation.load(Ordering::Acquire);
            let guard = self.hot_handles.load();
            if let Some((ref env, db, cached_gen)) = **guard {
                if cached_gen == current_gen {
                    return Ok((env.clone(), db));
                }
            }
        }
        self.ensure_open_handles()
    }

    fn validate_database(&self) -> Result<(), String> {
        self.reopen_if_closed()?;
        let _ = self.env.ensure_open()?;
        Ok(())
    }
}

impl Drop for LmdbDatabase {
    fn drop(&mut self) {
        {
            if let Ok(mut tx) = self.worker_tx.lock() {
                if let Some(sender) = tx.take() {
                    let _ = sender.send(WorkerCommand::Shutdown);
                }
            }
        }
        {
            if let Ok(mut wh) = self.worker_handle.lock() {
                if let Some(handle) = wh.take() {
                    let _ = handle.join();
                }
            }
        }

        let already_closed = self.state.lock().map(|s| s.closed).unwrap_or(false);

        if !already_closed {
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
    value: Binary,
) -> NifResult<Term<'a>> {
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    ensure_worker(&db_handle);
    if db_handle.has_fatal_error.load(Ordering::Acquire) {
        if let Ok(guard) = db_handle.fatal_error.lock() {
            if let Some(ref err) = *guard {
                return Ok(
                    (atoms::error(), atoms::transaction_error(), err.clone()).encode(env),
                );
            }
        }
    }

    let key_vec = key.as_slice().to_vec();
    let value_vec = value.as_slice().to_vec();

    if key_vec.is_empty() {
        let (live_env, live_db) = match db_handle.ensure_open_handles() {
            Ok(handles) => handles,
            Err(error_msg) => {
                return Ok(
                    (atoms::error(), atoms::database_error(), error_msg).encode(env),
                );
            }
        };
        let mut txn = match live_env.begin_rw_txn() {
            Ok(txn) => txn,
            Err(_) => {
                return Ok((
                    atoms::error(),
                    atoms::transaction_error(),
                    "Failed to begin write transaction".to_string(),
                )
                    .encode(env));
            }
        };
        match txn.put(live_db, &key_vec, &value_vec, WriteFlags::empty()) {
            Ok(()) => match txn.commit() {
                Ok(()) => return Ok(atoms::ok().encode(env)),
                Err(_) => {
                    return Ok((
                        atoms::error(),
                        atoms::transaction_error(),
                        "Failed to commit transaction".to_string(),
                    )
                        .encode(env))
                }
            },
            Err(lmdb_err) => {
                let error_msg = match lmdb_err {
                    lmdb::Error::BadValSize => "Empty key not supported".to_string(),
                    _ => format!("Failed to put value: {:?}", lmdb_err),
                };
                return Ok(
                    (atoms::error(), atoms::transaction_error(), error_msg).encode(env),
                );
            }
        }
    }

    loop {
        let map = db_handle.active.load();
        let _ = map.upsert_sync(key_vec.clone(), value_vec.clone());
        if Arc::ptr_eq(&map, &db_handle.active.load()) {
            break;
        }
    }

    let count = db_handle.op_count.fetch_add(1, Ordering::Relaxed) + 1;
    let threshold = db_handle.batch_size.load(Ordering::Relaxed);
    if count >= threshold
        && db_handle
            .flush_pending
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
    {
        send_background_flush_if_worker_present(&db_handle);
    }

    Ok(atoms::ok().encode(env))
}

#[rustler::nif]
fn get<'a>(
    env: Env<'a>,
    db_handle: &'a LmdbDatabase,
    key: Binary,
) -> NifResult<Term<'a>> {
    if db_handle.is_closed.load(Ordering::Relaxed) {
        if let Err(error_msg) = db_handle.validate_database() {
            return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
        }
    }

    if db_handle.has_fatal_error.load(Ordering::Relaxed) {
        if let Ok(guard) = db_handle.fatal_error.lock() {
            if let Some(ref err) = *guard {
                return Ok((atoms::error(), atoms::transaction_error(), err.clone()).encode(env));
            }
        }
    }

    let key_bytes = key.as_slice();

    // Fast path: skip overlay checks if both maps are empty
    let active_guard = db_handle.active.load();
    if !active_guard.is_empty() {
        if let Some(value) = active_guard.read_sync(key_bytes, |_, v| v.clone()) {
            let mut binary = OwnedBinary::new(value.len()).ok_or(Error::BadArg)?;
            binary.as_mut_slice().copy_from_slice(&value);
            return Ok((atoms::ok(), binary.release(env)).encode(env));
        }
    }

    {
        let draining_guard = db_handle.draining.load();
        if let Some(ref old_map) = **draining_guard {
            if let Some(value) = old_map.read_sync(key_bytes, |_, v| v.clone()) {
                let mut binary = OwnedBinary::new(value.len()).ok_or(Error::BadArg)?;
                binary.as_mut_slice().copy_from_slice(&value);
                return Ok((atoms::ok(), binary.release(env)).encode(env));
            }
        }
    }

    let (live_env, live_db) = match db_handle.fast_get_handles() {
        Ok(handles) => handles,
        Err(error_msg) => {
            return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
        }
    };

    let txn = match live_env.begin_ro_txn() {
        Ok(txn) => txn,
        Err(_) => {
            return Ok((
                atoms::error(),
                atoms::transaction_error(),
                "Failed to begin read transaction".to_string(),
            )
                .encode(env));
        }
    };

    match txn.get(live_db, &key_bytes) {
        Ok(value_bytes) => {
            let mut binary = OwnedBinary::new(value_bytes.len()).unwrap();
            binary.as_mut_slice().copy_from_slice(value_bytes);
            Ok((atoms::ok(), binary.release(env)).encode(env))
        }
        Err(lmdb::Error::NotFound) => Ok(atoms::not_found().encode(env)),
        Err(_) => Ok(
            (atoms::error(), atoms::database_error(), "Failed to get value".to_string())
                .encode(env),
        ),
    }
}

#[rustler::nif(schedule = "DirtyIo")]
fn flush<'a>(env: Env<'a>, db_handle: ResourceArc<LmdbDatabase>) -> NifResult<Term<'a>> {
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    ensure_worker(&db_handle);
    match flush_sync(&db_handle) {
        Ok(()) => Ok(atoms::ok().encode(env)),
        Err(error_msg) => {
            Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env))
        }
    }
}

#[rustler::nif]
fn overlay_count<'a>(env: Env<'a>, db_handle: &'a LmdbDatabase) -> NifResult<Term<'a>> {
    let active = db_handle.active.load();
    let count = active.len();
    Ok(count.encode(env))
}

#[rustler::nif]
fn put_batch<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    key_value_pairs: Vec<(Binary, Binary)>,
) -> NifResult<Term<'a>> {
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    ensure_worker(&db_handle);

    if key_value_pairs.is_empty() {
        return Ok(atoms::ok().encode(env));
    }

    if db_handle.has_fatal_error.load(Ordering::Acquire) {
        if let Ok(guard) = db_handle.fatal_error.lock() {
            if let Some(ref err) = *guard {
                return Ok((atoms::error(), atoms::transaction_error(), err.clone()).encode(env));
            }
        }
    }

    for (key, _value) in key_value_pairs.iter() {
        let klen = key.as_slice().len();
        if klen == 0 {
            return Ok((atoms::error(), atoms::validation_error(), "Empty key in batch".to_string()).encode(env));
        }
        if klen > LMDB_DEFAULT_MAX_KEY_SIZE {
            return Ok((atoms::error(), atoms::validation_error(), format!("Key size {klen} exceeds limit {LMDB_DEFAULT_MAX_KEY_SIZE}")).encode(env));
        }
    }

    loop {
        let m = db_handle.active.load();
        let _reserved = m.reserve(key_value_pairs.len());
        for (key, value) in key_value_pairs.iter() {
            let _ = m.upsert_sync(key.as_slice().to_vec(), value.as_slice().to_vec());
        }
        if Arc::ptr_eq(&m, &db_handle.active.load()) {
            break;
        }
    }
    let count = db_handle.op_count.fetch_add(key_value_pairs.len(), Ordering::Relaxed) + key_value_pairs.len();
    let threshold = db_handle.batch_size.load(Ordering::Relaxed);
    if count >= threshold
        && db_handle
            .flush_pending
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
    {
        send_background_flush_if_worker_present(&db_handle);
    }

    Ok(atoms::ok().encode(env))
}

///===================================================================
/// Iterator Operations
///===================================================================

#[rustler::nif]
fn iterator<'a>(env: Env<'a>, db_handle: &'a LmdbDatabase) -> NifResult<Term<'a>> {
    let _ = db_handle;
    Ok(encode_iterator_start(env))
}

#[rustler::nif(schedule = "DirtyIo")]
fn iterator_next<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    cursor_term: Term<'a>,
) -> NifResult<Term<'a>> {
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    ensure_worker(&db_handle);

    let cursor_token = match decode_iterator_cursor(cursor_term) {
        Ok(token) => token,
        Err(error_msg) => {
            return Ok((atoms::error(), atoms::invalid(), error_msg).encode(env));
        }
    };

    let active_empty = db_handle.active.load().is_empty();
    let draining_empty = db_handle.draining.load().is_none();
    if !active_empty || !draining_empty {
        if let Err(error_msg) = flush_sync(&db_handle) {
            return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
        }
    }

    let (live_env, live_db) = match db_handle.fast_get_handles() {
        Ok(handles) => handles,
        Err(error_msg) => {
            return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
        }
    };

    let txn = match live_env.begin_ro_txn() {
        Ok(txn) => txn,
        Err(_) => {
            return Ok((
                atoms::error(),
                atoms::transaction_error(),
                "Failed to begin read transaction".to_string(),
            )
                .encode(env));
        }
    };

    let cursor = match txn.open_ro_cursor(live_db) {
        Ok(cursor) => cursor,
        Err(_) => {
            return Ok((
                atoms::error(),
                atoms::database_error(),
                "Failed to open cursor".to_string(),
            )
                .encode(env));
        }
    };

    let next_entry = match cursor_token {
        IteratorCursor::Start => match cursor.get(None, None, MDB_FIRST) {
            Ok((Some(key), value)) => Some((key.to_vec(), value.to_vec())),
            Ok((None, _)) => None,
            Err(lmdb::Error::NotFound) => None,
            Err(_) => {
                return Ok((
                    atoms::error(),
                    atoms::database_error(),
                    "Failed to read first cursor entry".to_string(),
                )
                    .encode(env));
            }
        },
        IteratorCursor::AfterKey(last_key) => {
            let positioned_entry =
                match cursor.get(Some(last_key.as_slice()), None, MDB_SET_RANGE) {
                    Ok((Some(key), value)) => Some((key.to_vec(), value.to_vec())),
                    Ok((None, _)) => None,
                    Err(lmdb::Error::NotFound) => None,
                    Err(_) => {
                        return Ok((
                            atoms::error(),
                            atoms::database_error(),
                            "Failed to position iterator cursor".to_string(),
                        )
                            .encode(env));
                    }
                };

            match positioned_entry {
                Some((key, _value)) if key == last_key => {
                    match cursor.get(None, None, MDB_NEXT) {
                        Ok((Some(next_key), next_value)) => {
                            Some((next_key.to_vec(), next_value.to_vec()))
                        }
                        Ok((None, _)) => None,
                        Err(lmdb::Error::NotFound) => None,
                        Err(_) => {
                            return Ok((
                                atoms::error(),
                                atoms::database_error(),
                                "Failed to advance iterator cursor".to_string(),
                            )
                                .encode(env));
                        }
                    }
                }
                Some((key, value)) => Some((key, value)),
                None => None,
            }
        }
    };

    match next_entry {
        Some((key, value)) => {
            let key_term = encode_binary(env, &key)?;
            let value_term = encode_binary(env, &value)?;
            let next_cursor = encode_iterator_after_key(env, &key)?;
            Ok((atoms::ok(), key_term, value_term, next_cursor).encode(env))
        }
        None => Ok(atoms::undefined().encode(env)),
    }
}

///===================================================================
/// List Operations
///===================================================================

#[rustler::nif(schedule = "DirtyIo")]
fn list<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    key_prefix: Binary,
) -> NifResult<Term<'a>> {
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    ensure_worker(&db_handle);

    let prefix_bytes = key_prefix.as_slice();

    let active_empty = db_handle.active.load().is_empty();
    let draining_empty = db_handle.draining.load().is_none();
    if !active_empty || !draining_empty {
        if let Err(error_msg) = flush_sync(&db_handle) {
            return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
        }
    }

    let (live_env, live_db) = match db_handle.fast_get_handles() {
        Ok(handles) => handles,
        Err(error_msg) => {
            return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
        }
    };

    let txn = match live_env.begin_ro_txn() {
        Ok(txn) => txn,
        Err(_) => {
            return Ok((
                atoms::error(),
                atoms::transaction_error(),
                "Failed to begin read transaction".to_string(),
            )
                .encode(env));
        }
    };

    let mut cursor = match txn.open_ro_cursor(live_db) {
        Ok(cursor) => cursor,
        Err(_) => {
            return Ok((
                atoms::error(),
                atoms::database_error(),
                "Failed to open cursor".to_string(),
            )
                .encode(env));
        }
    };

    let mut children = Vec::with_capacity(64);
    let prefix_len = prefix_bytes.len();

    let cursor_positioned = cursor.get(Some(prefix_bytes), None, MDB_SET_RANGE).is_ok();

    if !cursor_positioned {
        return Ok(atoms::not_found().encode(env));
    }

    let cursor_iter = cursor.iter_from(prefix_bytes);

    for (key, _value) in cursor_iter {
        if !key.starts_with(prefix_bytes) {
            break;
        }

        let remaining = &key[prefix_len..];

        if remaining.is_empty() {
            continue;
        }

        let next_component = if let Some(sep_pos) = remaining.iter().position(|&b| b == b'/') {
            &remaining[..sep_pos]
        } else {
            remaining
        };

        if next_component.is_empty() {
            continue;
        }

        let component_exists = if children.len() < 16 {
            children
                .iter()
                .any(|existing: &Vec<u8>| existing.as_slice() == next_component)
        } else {
            children.binary_search(&next_component.to_vec()).is_ok()
        };

        if !component_exists {
            let component_vec = next_component.to_vec();
            if children.len() < 16 {
                children.push(component_vec);
            } else if let Err(pos) = children.binary_search(&component_vec) {
                children.insert(pos, component_vec);
            }
        }
    }

    if children.is_empty() {
        return Ok(atoms::not_found().encode(env));
    }

    if children.len() < 16 {
        children.sort_unstable();
    }

    let mut result_binaries = Vec::with_capacity(children.len());

    for child in children {
        let mut binary = OwnedBinary::new(child.len()).ok_or(Error::BadArg)?;
        binary.as_mut_slice().copy_from_slice(&child);
        result_binaries.push(binary.release(env));
    }

    Ok((atoms::ok(), result_binaries).encode(env))
}

#[rustler::nif(schedule = "DirtyIo")]
fn match_pattern<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    patterns: Vec<(Binary, Binary)>,
) -> NifResult<Term<'a>> {
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    ensure_worker(&db_handle);

    if patterns.is_empty() {
        return Ok(atoms::not_found().encode(env));
    }

    let patterns_vec: Vec<(&[u8], &[u8])> = patterns
        .iter()
        .map(|(k, v)| (k.as_slice(), v.as_slice()))
        .collect();

    let active_empty = db_handle.active.load().is_empty();
    let draining_empty = db_handle.draining.load().is_none();
    if !active_empty || !draining_empty {
        if let Err(error_msg) = flush_sync(&db_handle) {
            return Ok((atoms::error(), atoms::transaction_error(), error_msg).encode(env));
        }
    }

    let (live_env, live_db) = match db_handle.fast_get_handles() {
        Ok(handles) => handles,
        Err(error_msg) => {
            return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
        }
    };

    let txn = match live_env.begin_ro_txn() {
        Ok(txn) => txn,
        Err(_) => {
            return Ok((
                atoms::error(),
                atoms::transaction_error(),
                "Failed to begin read transaction".to_string(),
            )
                .encode(env));
        }
    };

    let mut cursor = match txn.open_ro_cursor(live_db) {
        Ok(cursor) => cursor,
        Err(_) => {
            return Ok((
                atoms::error(),
                atoms::database_error(),
                "Failed to open cursor".to_string(),
            )
                .encode(env));
        }
    };

    const MAX_RESULTS: usize = 100000;
    let mut matching_ids: Vec<Vec<u8>> = Vec::new();
    let mut current_id: Option<Vec<u8>> = None;
    let mut seen_patterns: HashSet<usize> = HashSet::new();
    let total_patterns = patterns_vec.len();

    let iter = cursor.iter_start();
    for (key_bytes, value_bytes) in iter {
        let last_slash_pos = key_bytes.iter().rposition(|&b| b == b'/');

        let (id, suffix) = if let Some(pos) = last_slash_pos {
            let id = key_bytes[..pos].to_vec();
            let suffix = key_bytes[pos + 1..].to_vec();
            (id, suffix)
        } else {
            (key_bytes.to_vec(), Vec::new())
        };

        if current_id.as_ref() != Some(&id) {
            if let Some(prev_id) = current_id.take() {
                if seen_patterns.len() == total_patterns {
                    matching_ids.push(prev_id);
                    if matching_ids.len() >= MAX_RESULTS {
                        break;
                    }
                }
            }

            current_id = Some(id.clone());
            seen_patterns.clear();
        }

        for (pattern_idx, (pattern_key, pattern_value)) in patterns_vec.iter().enumerate() {
            if suffix.as_slice() == *pattern_key && value_bytes == *pattern_value {
                seen_patterns.insert(pattern_idx);
            }
        }
    }

    if let Some(final_id) = current_id {
        if seen_patterns.len() == total_patterns {
            matching_ids.push(final_id);
        }
    }

    if matching_ids.is_empty() {
        Ok(atoms::not_found().encode(env))
    } else {
        let mut result_binaries = Vec::with_capacity(matching_ids.len());
        for id in matching_ids {
            let mut binary = OwnedBinary::new(id.len()).ok_or(Error::BadArg)?;
            binary.as_mut_slice().copy_from_slice(&id);
            result_binaries.push(binary.release(env));
        }

        Ok((atoms::ok(), result_binaries).encode(env))
    }
}

///===================================================================
/// Helper Functions
///===================================================================
fn encode_binary<'a>(env: Env<'a>, bytes: &[u8]) -> NifResult<Term<'a>> {
    let mut binary = OwnedBinary::new(bytes.len()).ok_or(Error::BadArg)?;
    binary.as_mut_slice().copy_from_slice(bytes);
    Ok(binary.release(env).encode(env))
}

fn send_background_flush_if_worker_present(db_handle: &LmdbDatabase) {
    if let Ok(tx) = db_handle.worker_tx.lock() {
        if let Some(ref sender) = *tx {
            let _ = sender.send(WorkerCommand::Flush);
        }
    }
}

fn encode_iterator_start<'a>(env: Env<'a>) -> Term<'a> {
    (atoms::iterator(), atoms::start()).encode(env)
}

fn encode_iterator_after_key<'a>(env: Env<'a>, key: &[u8]) -> NifResult<Term<'a>> {
    Ok((atoms::iterator(), encode_binary(env, key)?).encode(env))
}

fn decode_iterator_cursor(cursor_term: Term) -> Result<IteratorCursor, String> {
    let (tag, payload): (rustler::Atom, Term) = cursor_term
        .decode()
        .map_err(|_| "Invalid iterator cursor format".to_string())?;

    if tag != atoms::iterator() {
        return Err("Invalid iterator cursor tag".to_string());
    }

    if let Ok(atom_payload) = payload.decode::<rustler::Atom>() {
        if atom_payload == atoms::start() {
            return Ok(IteratorCursor::Start);
        }
    }

    if let Ok(binary_payload) = payload.decode::<Binary>() {
        return Ok(IteratorCursor::AfterKey(binary_payload.as_slice().to_vec()));
    }

    Err("Invalid iterator cursor payload".to_string())
}

fn lmdb_error_to_atom(error: lmdb::Error) -> rustler::Atom {
    match error {
        lmdb::Error::KeyExist => atoms::key_exist(),
        lmdb::Error::NotFound => atoms::not_found(),
        lmdb::Error::PageNotFound => atoms::page_not_found(),
        lmdb::Error::Corrupted => atoms::corrupted(),
        lmdb::Error::Panic => atoms::panic(),
        lmdb::Error::VersionMismatch => atoms::version_mismatch(),
        lmdb::Error::Invalid => atoms::invalid(),
        lmdb::Error::MapFull => atoms::map_full(),
        lmdb::Error::DbsFull => atoms::dbs_full(),
        lmdb::Error::ReadersFull => atoms::readers_full(),
        lmdb::Error::TlsFull => atoms::tls_full(),
        lmdb::Error::TxnFull => atoms::txn_full(),
        lmdb::Error::CursorFull => atoms::cursor_full(),
        lmdb::Error::PageFull => atoms::page_full(),
        lmdb::Error::MapResized => atoms::map_resized(),
        lmdb::Error::Incompatible => atoms::incompatible(),
        lmdb::Error::BadRslot => atoms::bad_rslot(),
        lmdb::Error::BadTxn => atoms::bad_txn(),
        lmdb::Error::BadValSize => atoms::bad_val_size(),
        lmdb::Error::BadDbi => atoms::bad_dbi(),
        lmdb::Error::Other(28) => atoms::no_space(),
        lmdb::Error::Other(_) => atoms::io_error(),
    }
}

fn parse_env_options(options: Vec<Term>) -> NifResult<EnvOptions> {
    let mut env_opts = EnvOptions::default();

    for option in options {
        if let Ok((atom, value)) = option.decode::<(rustler::Atom, Term)>() {
            let name = format!("{:?}", atom);
            let name = name.trim_start_matches('"').trim_end_matches('"');
            match name {
                "map_size" => {
                    if let Ok(size) = value.decode::<u64>() {
                        env_opts.map_size = Some(size);
                    }
                }
                "max_readers" => {
                    if let Ok(readers) = value.decode::<u32>() {
                        env_opts.max_readers = Some(readers);
                    }
                }
                "batch_size" => {
                    if let Ok(size) = value.decode::<u64>() {
                        if size > 0 && size <= usize::MAX as u64 {
                            env_opts.batch_size = Some(size as usize);
                        }
                    }
                }
                _ => {}
            }
        } else if let Ok(atom) = option.decode::<rustler::Atom>() {
            let name = format!("{:?}", atom);
            let name = name.trim_start_matches('"').trim_end_matches('"');
            match name {
                "no_mem_init" => env_opts.no_mem_init = true,
                "no_sync" => env_opts.no_sync = true,
                "no_lock" => env_opts.no_lock = true,
                "write_map" => env_opts.write_map = true,
                "no_readahead" => env_opts.no_readahead = true,
                _ => {}
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
            let name = name.trim_start_matches('"').trim_end_matches('"');
            if name == "create" {
                db_opts.create = true;
            }
        }
    }

    Ok(db_opts)
}

#[derive(Debug, Default, Clone)]
struct EnvOptions {
    map_size: Option<u64>,
    max_readers: Option<u32>,
    batch_size: Option<usize>,
    no_mem_init: bool,
    no_sync: bool,
    no_lock: bool,
    write_map: bool,
    no_readahead: bool,
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
    let closed = env_handle.is_closed().map_err(|_| Error::BadArg)?;

    let ref_count = {
        let ref_count = env_handle.ref_count.lock().map_err(|_| Error::BadArg)?;
        *ref_count
    };

    Ok((atoms::ok(), closed, ref_count, env_handle.path.clone()).encode(env))
}

rustler::init!("elmdb", load = init);
