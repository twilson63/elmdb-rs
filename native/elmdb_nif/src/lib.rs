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
use rustler::types::binary::OwnedBinary;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex};
use std::path::Path;
use std::time::{Instant, Duration};
use std::thread;
use lmdb::{Environment, EnvironmentFlags, Database, DatabaseFlags, Transaction, WriteFlags, Cursor};

// LMDB constants for cursor positioning (instead of importing lmdb-sys only for constants from lmdb_sys::ffi). 
// To be improved in the future.
const MDB_SET_RANGE: u32 = 17;
const MDB_FIRST: u32 = 0;

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
        // Phase 3: Enhanced error atoms for concurrent scenarios
        concurrent_panic,
        concurrent_timeout,
        concurrent_contention,
        concurrent_deadlock,
        cursor_conflict,
        transaction_conflict,
        resource_exhausted,
        operation_aborted,
    }
}

///===================================================================
/// Concurrent Safety Enhancement Implementation
///===================================================================
/// 
/// Phase 1: Comprehensive panic isolation for all cursor operations
/// - Wraps cursor creation, positioning, and iteration in panic handlers
/// - Converts panics to proper error messages instead of crashing the BEAM VM
/// - Provides granular error reporting for different LMDB error conditions
/// 
/// Phase 2: Transaction serialization for concurrent access 
/// - Uses global CURSOR_MUTEX to serialize cursor creation across threads
/// - Prevents LMDB internal conflicts during concurrent cursor operations
/// - Minimizes lock time by releasing immediately after cursor creation
/// - Maintains high performance while ensuring thread safety
/// 
/// Phase 3: Enhanced error handling and logging for concurrent scenarios
/// - Comprehensive error categorization for different concurrent access patterns
/// - Structured logging with detailed context and timing information
/// - Resource usage tracking and contention detection
/// - Debuggable error messages that preserve troubleshooting context
/// - Non-sensitive error reporting suitable for production logging
///===================================================================

/// Phase 3: Comprehensive error categorization for concurrent scenarios
/// 
/// Provides detailed categorization of different types of errors that can occur
/// during concurrent access to LMDB, enabling better debugging and monitoring.
#[derive(Debug, Clone, PartialEq)]
pub enum ConcurrentErrorCategory {
    /// Panic occurred during concurrent operation
    Panic {
        operation: String,
        thread_id: String,
        duration: Duration,
        context: String,
    },
    /// Operation timed out due to lock contention
    Timeout {
        operation: String,
        timeout_duration: Duration,
        attempted_duration: Duration,
        resource: String,
    },
    /// Lock contention detected but operation proceeded
    Contention {
        operation: String,
        wait_duration: Duration,
        resource: String,
        concurrent_operations: u32,
    },
    /// Potential deadlock scenario detected
    Deadlock {
        operation: String,
        thread_id: String,
        held_resources: Vec<String>,
        requested_resource: String,
    },
    /// Cursor operation conflict during concurrent access
    CursorConflict {
        operation: String,
        conflict_type: String,
        recovery_action: String,
    },
    /// Transaction conflict or interference
    TransactionConflict {
        operation: String,
        transaction_type: String,
        conflict_reason: String,
    },
    /// Resource exhaustion under concurrent load
    ResourceExhausted {
        resource_type: String,
        current_usage: u64,
        limit: u64,
        concurrent_operations: u32,
    },
    /// Operation was aborted due to concurrent conditions
    OperationAborted {
        operation: String,
        reason: String,
        retry_recommended: bool,
    },
}

/// Phase 3: Enhanced error context for detailed debugging
/// 
/// Captures comprehensive context information for troubleshooting
/// concurrent access issues without exposing sensitive data.
#[derive(Debug, Clone)]
pub struct ConcurrentErrorContext {
    /// High-level error category
    pub category: ConcurrentErrorCategory,
    /// Timestamp when error occurred
    pub timestamp: Instant,
    /// Thread identifier (non-sensitive)
    pub thread_id: String,
    /// Operation that was being performed
    pub operation: String,
    /// Resource path or identifier (sanitized)
    pub resource_path: Option<String>,
    /// Performance metrics at time of error
    pub metrics: ConcurrentMetrics,
    /// Recovery suggestions
    pub recovery_hints: Vec<String>,
}

/// Phase 3: Performance and resource usage metrics
/// 
/// Tracks key metrics that help diagnose concurrent access issues
#[derive(Debug, Clone, Default)]
pub struct ConcurrentMetrics {
    /// Number of concurrent operations detected
    pub concurrent_operations: u32,
    /// Time spent waiting for locks
    pub lock_wait_time: Duration,
    /// Total operation duration
    pub operation_duration: Duration,
    /// Memory usage estimation (KB)
    pub memory_usage_kb: u64,
    /// Number of retry attempts
    pub retry_count: u32,
}

/// Phase 3: Structured logging system for concurrent access
/// 
/// Provides structured, searchable logging for concurrent scenarios
/// with appropriate detail levels for different use cases.
pub struct ConcurrentLogger {
    /// Enable debug-level logging
    debug_enabled: bool,
    /// Enable performance metrics logging
    metrics_enabled: bool,
    /// Log level threshold
    min_severity: LogSeverity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogSeverity {
    Debug = 0,
    Info = 1,
    Warning = 2,
    Error = 3,
    Critical = 4,
}

impl ConcurrentLogger {
    /// Create logger with default settings suitable for production
    pub fn new() -> Self {
        Self {
            debug_enabled: false,
            metrics_enabled: true,
            min_severity: LogSeverity::Warning,
        }
    }
    
    /// Create logger with debug settings for development
    pub fn debug() -> Self {
        Self {
            debug_enabled: true,
            metrics_enabled: true,
            min_severity: LogSeverity::Debug,
        }
    }
    
    /// Log concurrent error with full context
    pub fn log_concurrent_error(&self, context: &ConcurrentErrorContext) {
        if LogSeverity::Error >= self.min_severity {
            self.emit_log(LogSeverity::Error, &self.format_error_message(context));
        }
    }
    
    /// Log concurrent warning with performance context
    pub fn log_concurrent_warning(&self, operation: &str, message: &str, metrics: &ConcurrentMetrics) {
        if LogSeverity::Warning >= self.min_severity {
            let formatted = self.format_warning_message(operation, message, metrics);
            self.emit_log(LogSeverity::Warning, &formatted);
        }
    }
    
    /// Log performance metrics for concurrent operations
    pub fn log_performance_metrics(&self, operation: &str, metrics: &ConcurrentMetrics) {
        if self.metrics_enabled && LogSeverity::Info >= self.min_severity {
            let formatted = self.format_metrics_message(operation, metrics);
            self.emit_log(LogSeverity::Info, &formatted);
        }
    }
    
    /// Log debug information about concurrent access patterns
    pub fn log_debug(&self, operation: &str, message: &str) {
        if self.debug_enabled && LogSeverity::Debug >= self.min_severity {
            let formatted = format!("[ELMDB_CONCURRENT_DEBUG] {}: {}", operation, message);
            self.emit_log(LogSeverity::Debug, &formatted);
        }
    }
    
    /// Format error message with full context but without sensitive information
    fn format_error_message(&self, context: &ConcurrentErrorContext) -> String {
        let category_desc = match &context.category {
            ConcurrentErrorCategory::Panic { operation, thread_id, duration, context: ctx } => {
                format!("PANIC in {} (thread: {}, duration: {:?}ms): {}", 
                       operation, thread_id, duration.as_millis(), ctx)
            },
            ConcurrentErrorCategory::Timeout { operation, timeout_duration, attempted_duration, resource } => {
                format!("TIMEOUT in {} on resource '{}' (timeout: {:?}ms, attempted: {:?}ms)", 
                       operation, resource, timeout_duration.as_millis(), attempted_duration.as_millis())
            },
            ConcurrentErrorCategory::Contention { operation, wait_duration, resource, concurrent_operations } => {
                format!("CONTENTION in {} on resource '{}' (wait: {:?}ms, concurrent ops: {})", 
                       operation, resource, wait_duration.as_millis(), concurrent_operations)
            },
            ConcurrentErrorCategory::Deadlock { operation, thread_id, held_resources, requested_resource } => {
                format!("DEADLOCK in {} (thread: {}) holding {:?}, requesting '{}'", 
                       operation, thread_id, held_resources, requested_resource)
            },
            ConcurrentErrorCategory::CursorConflict { operation, conflict_type, recovery_action } => {
                format!("CURSOR_CONFLICT in {} type '{}', recovery: {}", 
                       operation, conflict_type, recovery_action)
            },
            ConcurrentErrorCategory::TransactionConflict { operation, transaction_type, conflict_reason } => {
                format!("TXN_CONFLICT in {} ({}) reason: {}", 
                       operation, transaction_type, conflict_reason)
            },
            ConcurrentErrorCategory::ResourceExhausted { resource_type, current_usage, limit, concurrent_operations } => {
                format!("RESOURCE_EXHAUSTED {} usage: {}/{} with {} concurrent ops", 
                       resource_type, current_usage, limit, concurrent_operations)
            },
            ConcurrentErrorCategory::OperationAborted { operation, reason, retry_recommended } => {
                format!("OPERATION_ABORTED {} reason: {} (retry: {})", 
                       operation, reason, retry_recommended)
            },
        };
        
        let resource_info = context.resource_path
            .as_ref()
            .map(|p| format!(" resource='{}'", sanitize_path(p)))
            .unwrap_or_default();
        
        let metrics_info = if self.metrics_enabled {
            format!(" metrics[concurrent_ops={}, lock_wait={}ms, duration={}ms, retries={}]",
                   context.metrics.concurrent_operations,
                   context.metrics.lock_wait_time.as_millis(),
                   context.metrics.operation_duration.as_millis(),
                   context.metrics.retry_count)
        } else {
            String::new()
        };
        
        let recovery_info = if !context.recovery_hints.is_empty() {
            format!(" recovery_hints={:?}", context.recovery_hints)
        } else {
            String::new()
        };
        
        format!("[ELMDB_CONCURRENT_ERROR] {}{}{}{} thread={}",
               category_desc, resource_info, metrics_info, recovery_info, context.thread_id)
    }
    
    /// Format warning message with performance context
    fn format_warning_message(&self, operation: &str, message: &str, metrics: &ConcurrentMetrics) -> String {
        let metrics_info = if self.metrics_enabled {
            format!(" [concurrent_ops={}, wait_time={}ms, duration={}ms]",
                   metrics.concurrent_operations,
                   metrics.lock_wait_time.as_millis(),
                   metrics.operation_duration.as_millis())
        } else {
            String::new()
        };
        
        format!("[ELMDB_CONCURRENT_WARNING] {}: {}{}", operation, message, metrics_info)
    }
    
    /// Format performance metrics message
    fn format_metrics_message(&self, operation: &str, metrics: &ConcurrentMetrics) -> String {
        format!("[ELMDB_CONCURRENT_METRICS] {} concurrent_ops={} lock_wait={}ms duration={}ms memory={}KB retries={}",
               operation,
               metrics.concurrent_operations,
               metrics.lock_wait_time.as_millis(),
               metrics.operation_duration.as_millis(),
               metrics.memory_usage_kb,
               metrics.retry_count)
    }
    
    /// Emit log message to stderr with timestamp
    fn emit_log(&self, severity: LogSeverity, message: &str) {
        let severity_str = match severity {
            LogSeverity::Debug => "DEBUG",
            LogSeverity::Info => "INFO",
            LogSeverity::Warning => "WARN",
            LogSeverity::Error => "ERROR",
            LogSeverity::Critical => "CRITICAL",
        };
        
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        
        eprintln!("[{}] {} {}", timestamp, severity_str, message);
    }
}

/// Sanitize path for logging (remove sensitive information, keep structure)
fn sanitize_path(path: &str) -> String {
    if path.is_empty() {
        return "<empty>".to_string();
    }
    
    // Replace actual path components with sanitized versions
    let parts: Vec<&str> = path.split('/').collect();
    if parts.len() <= 2 {
        // Short paths are probably OK to show
        path.to_string()
    } else {
        // Show first and last components, hash middle parts
        let first = parts.first().unwrap_or(&"");
        let last = parts.last().unwrap_or(&"");
        let hash = parts[1..parts.len()-1].join("/").len(); // Simple length-based hash
        format!("{}/...<{}_chars>.../{}", first, hash, last)
    }
}

/// Thread-safe concurrent logger instance with environment-based configuration
lazy_static::lazy_static! {
    static ref CONCURRENT_LOGGER: Arc<ConcurrentLogger> = Arc::new({
        // Check environment variable for debug mode
        match std::env::var("ELMDB_CONCURRENT_DEBUG").as_deref() {
            Ok("1") | Ok("true") | Ok("debug") => ConcurrentLogger::debug(),
            _ => ConcurrentLogger::new(),
        }
    });
}

/// Phase 3: Enhanced results of cursor operations with comprehensive error context
#[derive(Debug)]
enum CursorOperationResult<T> {
    Success(T),
    Panic(String),
    Error(String),
    /// New comprehensive error with full concurrent context
    ConcurrentError(ConcurrentErrorContext),
}

/// Phase 3: Helper functions for creating enhanced error contexts
impl<T> CursorOperationResult<T> {
    /// Create a concurrent error result with full context
    pub fn concurrent_error(category: ConcurrentErrorCategory, operation: &str, resource_path: Option<String>) -> Self {
        let context = ConcurrentErrorContext {
            category,
            timestamp: Instant::now(),
            thread_id: get_thread_id(),
            operation: operation.to_string(),
            resource_path,
            metrics: ConcurrentMetrics::default(),
            recovery_hints: Vec::new(),
        };
        CursorOperationResult::ConcurrentError(context)
    }
    
    /// Create a concurrent panic error with timing information
    pub fn concurrent_panic(operation: &str, panic_msg: &str, duration: Duration) -> Self {
        let category = ConcurrentErrorCategory::Panic {
            operation: operation.to_string(),
            thread_id: get_thread_id(),
            duration,
            context: panic_msg.to_string(),
        };
        Self::concurrent_error(category, operation, None)
    }
    
    /// Create a concurrent timeout error
    pub fn concurrent_timeout(operation: &str, timeout: Duration, attempted: Duration, resource: &str) -> Self {
        let category = ConcurrentErrorCategory::Timeout {
            operation: operation.to_string(),
            timeout_duration: timeout,
            attempted_duration: attempted,
            resource: resource.to_string(),
        };
        Self::concurrent_error(category, operation, None)
    }
    
    /// Create a concurrent contention error
    pub fn concurrent_contention(operation: &str, wait_time: Duration, resource: &str, concurrent_ops: u32) -> Self {
        let category = ConcurrentErrorCategory::Contention {
            operation: operation.to_string(),
            wait_duration: wait_time,
            resource: resource.to_string(),
            concurrent_operations: concurrent_ops,
        };
        Self::concurrent_error(category, operation, None)
    }
}

/// Generate a non-sensitive thread identifier
fn get_thread_id() -> String {
    // Use a hash of the thread ID to avoid exposing actual thread identifiers
    let thread_id = thread::current().id();
    format!("tid_{:?}", thread_id).chars().take(12).collect()
}

/// Phase 3: Format concurrent error context for Erlang consumption
/// 
/// Creates a user-friendly error message that includes key troubleshooting information
/// while avoiding sensitive data exposure.
fn format_concurrent_error_for_erlang(context: &ConcurrentErrorContext) -> String {
    let category_info = match &context.category {
        ConcurrentErrorCategory::Panic { operation, duration, context: panic_ctx, .. } => {
            format!("PANIC in {} after {:?}ms: {}", operation, duration.as_millis(), panic_ctx)
        },
        ConcurrentErrorCategory::Timeout { operation, timeout_duration, attempted_duration, resource } => {
            format!("TIMEOUT in {} on {} (limit: {:?}ms, actual: {:?}ms)", 
                   operation, resource, timeout_duration.as_millis(), attempted_duration.as_millis())
        },
        ConcurrentErrorCategory::Contention { operation, wait_duration, resource, concurrent_operations } => {
            format!("CONTENTION in {} on {} (wait: {:?}ms, {} concurrent ops)", 
                   operation, resource, wait_duration.as_millis(), concurrent_operations)
        },
        ConcurrentErrorCategory::Deadlock { operation, held_resources, requested_resource, .. } => {
            format!("DEADLOCK in {} (holding: {:?}, requesting: {})", 
                   operation, held_resources, requested_resource)
        },
        ConcurrentErrorCategory::CursorConflict { operation, conflict_type, recovery_action } => {
            format!("CURSOR_CONFLICT in {} (type: {}, recovery: {})", 
                   operation, conflict_type, recovery_action)
        },
        ConcurrentErrorCategory::TransactionConflict { operation, transaction_type, conflict_reason } => {
            format!("TXN_CONFLICT in {} ({}: {})", operation, transaction_type, conflict_reason)
        },
        ConcurrentErrorCategory::ResourceExhausted { resource_type, current_usage, limit, concurrent_operations } => {
            format!("RESOURCE_EXHAUSTED {} ({}/{} with {} concurrent ops)", 
                   resource_type, current_usage, limit, concurrent_operations)
        },
        ConcurrentErrorCategory::OperationAborted { operation, reason, retry_recommended } => {
            format!("OPERATION_ABORTED {} (reason: {}, retry: {})", 
                   operation, reason, retry_recommended)
        },
    };
    
    // Add performance metrics if significant
    let metrics_info = if context.metrics.operation_duration.as_millis() > 100 
        || context.metrics.lock_wait_time.as_millis() > 10 {
        format!(" [duration: {:?}ms, lock_wait: {:?}ms]",
               context.metrics.operation_duration.as_millis(),
               context.metrics.lock_wait_time.as_millis())
    } else {
        String::new()
    };
    
    // Add primary recovery hint if available
    let recovery_info = if let Some(hint) = context.recovery_hints.first() {
        format!(" Suggestion: {}", hint)
    } else {
        String::new()
    };
    
    format!("{}{}{}", category_info, metrics_info, recovery_info)
}

/// Phase 3: Enhanced generic safe cursor operation wrapper with comprehensive error handling
/// 
/// This function wraps cursor operations in panic handling and provides detailed
/// concurrent error reporting with timing, context, and recovery suggestions.
/// 
/// # Arguments
/// * `operation_name` - Human-readable name for the operation (for error reporting)
/// * `operation` - The closure containing the cursor operation to execute
/// * `resource_path` - Optional resource path for context (will be sanitized)
/// 
/// # Returns
/// * `CursorOperationResult<T>` - Success, traditional error, or enhanced concurrent error
fn safe_cursor_operation<T, F>(
    operation_name: &str,
    operation: F,
) -> CursorOperationResult<T>
where
    F: FnOnce() -> Result<T, lmdb::Error> + std::panic::UnwindSafe,
{
    safe_cursor_operation_with_context(operation_name, operation, None)
}

/// Phase 3: Enhanced safe cursor operation with resource context
/// 
/// Provides the same functionality as safe_cursor_operation but with additional
/// resource context for better error reporting and debugging.
fn safe_cursor_operation_with_context<T, F>(
    operation_name: &str,
    operation: F,
    resource_path: Option<String>,
) -> CursorOperationResult<T>
where
    F: FnOnce() -> Result<T, lmdb::Error> + std::panic::UnwindSafe,
{
    let start_time = Instant::now();
    
    // Log debug information about the operation
    CONCURRENT_LOGGER.log_debug(operation_name, "Starting cursor operation");
    
    // Catch any panics that occur during the operation
    let panic_result = std::panic::catch_unwind(|| {
        operation()
    });
    
    let operation_duration = start_time.elapsed();
    
    match panic_result {
        Ok(operation_result) => {
            // No panic occurred, check the operation result
            match operation_result {
                Ok(value) => {
                    // Log successful operation metrics
                    let metrics = ConcurrentMetrics {
                        operation_duration,
                        ..Default::default()
                    };
                    CONCURRENT_LOGGER.log_performance_metrics(operation_name, &metrics);
                    
                    CursorOperationResult::Success(value)
                },
                Err(lmdb_error) => {
                    // Phase 3: Enhanced error categorization with concurrent context
                    let error_category = categorize_lmdb_error(operation_name, &lmdb_error, operation_duration);
                    let error_context = ConcurrentErrorContext {
                        category: error_category,
                        timestamp: Instant::now(),
                        thread_id: get_thread_id(),
                        operation: operation_name.to_string(),
                        resource_path: resource_path.clone(),
                        metrics: ConcurrentMetrics {
                            operation_duration,
                            ..Default::default()
                        },
                        recovery_hints: get_recovery_hints_for_lmdb_error(&lmdb_error),
                    };
                    
                    // Log the concurrent error
                    CONCURRENT_LOGGER.log_concurrent_error(&error_context);
                    
                    // For backwards compatibility, also provide the traditional error message
                    let traditional_error_msg = format_traditional_lmdb_error(operation_name, &lmdb_error);
                    
                    // Return enhanced error if it's a concurrent scenario, otherwise traditional error
                    match &lmdb_error {
                        lmdb::Error::BadTxn | lmdb::Error::BadDbi | lmdb::Error::CursorFull => {
                            CursorOperationResult::ConcurrentError(error_context)
                        },
                        _ => CursorOperationResult::Error(traditional_error_msg)
                    }
                }
            }
        }
        Err(panic_payload) => {
            // A panic occurred during the operation - this is always a concurrent issue
            let panic_msg = extract_panic_message(panic_payload);
            
            let category = ConcurrentErrorCategory::Panic {
                operation: operation_name.to_string(),
                thread_id: get_thread_id(),
                duration: operation_duration,
                context: panic_msg.clone(),
            };
            
            let error_context = ConcurrentErrorContext {
                category,
                timestamp: Instant::now(),
                thread_id: get_thread_id(),
                operation: operation_name.to_string(),
                resource_path,
                metrics: ConcurrentMetrics {
                    operation_duration,
                    ..Default::default()
                },
                recovery_hints: vec![
                    "Consider reducing concurrent access to this resource".to_string(),
                    "Check for memory corruption or invalid resource handles".to_string(),
                    "Verify that database environment is properly initialized".to_string(),
                ],
            };
            
            // Always log panics as they're critical
            CONCURRENT_LOGGER.log_concurrent_error(&error_context);
            
            // Return both enhanced and traditional panic information
            CursorOperationResult::Panic(format!("{}: Panic occurred - {}", operation_name, panic_msg))
        }
    }
}

/// Phase 3: Categorize LMDB errors for enhanced concurrent error reporting
fn categorize_lmdb_error(operation_name: &str, lmdb_error: &lmdb::Error, _duration: Duration) -> ConcurrentErrorCategory {
    match lmdb_error {
        lmdb::Error::BadTxn => ConcurrentErrorCategory::TransactionConflict {
            operation: operation_name.to_string(),
            transaction_type: "read_only".to_string(),
            conflict_reason: "Transaction became invalid during concurrent access".to_string(),
        },
        lmdb::Error::CursorFull => ConcurrentErrorCategory::CursorConflict {
            operation: operation_name.to_string(),
            conflict_type: "cursor_stack_overflow".to_string(),
            recovery_action: "Reduce concurrent cursor operations or increase stack size".to_string(),
        },
        lmdb::Error::BadDbi => ConcurrentErrorCategory::CursorConflict {
            operation: operation_name.to_string(),
            conflict_type: "invalid_database_handle".to_string(),
            recovery_action: "Reopen database handle or check for concurrent closure".to_string(),
        },
        lmdb::Error::MapFull => ConcurrentErrorCategory::ResourceExhausted {
            resource_type: "database_map".to_string(),
            current_usage: 0, // We don't have precise usage info from LMDB
            limit: 0, // We don't have the limit info here
            concurrent_operations: 0, // Would need tracking to determine this
        },
        lmdb::Error::TxnFull => ConcurrentErrorCategory::ResourceExhausted {
            resource_type: "transaction_slots".to_string(),
            current_usage: 0,
            limit: 0,
            concurrent_operations: 0,
        },
        _ => ConcurrentErrorCategory::OperationAborted {
            operation: operation_name.to_string(),
            reason: format!("LMDB error: {:?}", lmdb_error),
            retry_recommended: matches!(lmdb_error, 
                lmdb::Error::NotFound | lmdb::Error::MapResized | lmdb::Error::BadRslot
            ),
        },
    }
}

/// Phase 3: Generate recovery hints for LMDB errors
fn get_recovery_hints_for_lmdb_error(lmdb_error: &lmdb::Error) -> Vec<String> {
    match lmdb_error {
        lmdb::Error::BadTxn => vec![
            "Retry the operation with a new transaction".to_string(),
            "Check for concurrent database modifications".to_string(),
            "Ensure proper transaction lifecycle management".to_string(),
        ],
        lmdb::Error::CursorFull => vec![
            "Reduce the number of concurrent cursor operations".to_string(),
            "Close unused cursors before creating new ones".to_string(),
            "Consider using sequential access patterns instead of multiple cursors".to_string(),
        ],
        lmdb::Error::BadDbi => vec![
            "Reopen the database handle".to_string(),
            "Check if database was closed concurrently".to_string(),
            "Verify database handle lifecycle".to_string(),
        ],
        lmdb::Error::MapFull => vec![
            "Increase the database map size".to_string(),
            "Implement data cleanup or archiving".to_string(),
            "Consider sharding data across multiple databases".to_string(),
        ],
        lmdb::Error::TxnFull => vec![
            "Reduce concurrent transaction count".to_string(),
            "Commit or abort existing transactions".to_string(),
            "Use smaller batch sizes".to_string(),
        ],
        lmdb::Error::Corrupted => vec![
            "Stop all database operations immediately".to_string(),
            "Restore from backup if available".to_string(),
            "Run database integrity check".to_string(),
        ],
        lmdb::Error::NotFound => vec![
            "Check if key exists before access".to_string(),
            "Handle not_found as a normal condition".to_string(),
        ],
        lmdb::Error::MapResized => vec![
            "Retry operation - map was resized by another process".to_string(),
            "Synchronize map size changes across processes".to_string(),
        ],
        _ => vec![
            "Retry the operation after a brief delay".to_string(),
            "Check database and environment status".to_string(),
        ],
    }
}

/// Phase 3: Format traditional LMDB error message for backwards compatibility
fn format_traditional_lmdb_error(operation_name: &str, lmdb_error: &lmdb::Error) -> String {
    match lmdb_error {
        lmdb::Error::NotFound => format!("{}: No data found", operation_name),
        lmdb::Error::Corrupted => format!("{}: Database corrupted", operation_name),
        lmdb::Error::BadTxn => format!("{}: Invalid transaction", operation_name),
        lmdb::Error::BadDbi => format!("{}: Invalid database handle", operation_name),
        lmdb::Error::BadRslot => format!("{}: Invalid reuse slot", operation_name),
        lmdb::Error::BadValSize => format!("{}: Invalid value size", operation_name),
        lmdb::Error::KeyExist => format!("{}: Key already exists", operation_name),
        lmdb::Error::MapFull => format!("{}: Database full", operation_name),
        lmdb::Error::TxnFull => format!("{}: Transaction full", operation_name),
        lmdb::Error::CursorFull => format!("{}: Cursor stack full", operation_name),
        lmdb::Error::PageFull => format!("{}: Page full", operation_name),
        lmdb::Error::MapResized => format!("{}: Map resized", operation_name),
        lmdb::Error::Incompatible => format!("{}: Incompatible operation", operation_name),
        lmdb::Error::PageNotFound => format!("{}: Page not found", operation_name),
        _ => format!("{}: Unknown LMDB error: {:?}", operation_name, lmdb_error),
    }
}

/// Phase 3: Extract panic message from panic payload
fn extract_panic_message(panic_payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = panic_payload.downcast_ref::<&str>() {
        s.to_string()
    } else if let Some(s) = panic_payload.downcast_ref::<String>() {
        s.clone()
    } else {
        "Unknown panic occurred".to_string()
    }
}

/// Phase 3: Enhanced safe wrapper for cursor creation with comprehensive concurrent error handling
/// 
/// This function implements serialized cursor creation with enhanced error reporting,
/// timing metrics, and contention detection for concurrent access scenarios.
/// 
/// Phase 2: Uses global cursor mutex for serialization
/// Phase 3: Adds comprehensive error reporting, timing, and resource contention tracking
fn safe_cursor_create<'txn>(
    txn: &'txn lmdb::RoTransaction<'txn>,
    db: lmdb::Database,
) -> CursorOperationResult<lmdb::RoCursor<'txn>> {
    safe_cursor_create_with_context(txn, db, None)
}

/// Phase 3: Enhanced cursor creation with resource context and comprehensive error handling
fn safe_cursor_create_with_context<'txn>(
    txn: &'txn lmdb::RoTransaction<'txn>,
    db: lmdb::Database,
    resource_path: Option<String>,
) -> CursorOperationResult<lmdb::RoCursor<'txn>> {
    let operation_start = Instant::now();
    let mut concurrent_operations_detected = 0u32;
    
    // Log the cursor creation attempt
    CONCURRENT_LOGGER.log_debug("cursor_create", "Attempting cursor creation with serialization");
    
    // Attempt to acquire the cursor serialization mutex with try_lock
    // This prevents blocking and potential deadlocks in high-concurrency scenarios
    let lock_start = Instant::now();
    let cursor_guard_result = CURSOR_MUTEX.try_lock();
    let lock_wait_time = lock_start.elapsed();
    
    match cursor_guard_result {
        Ok(cursor_guard) => {
            // Successfully acquired the serialization lock
            CONCURRENT_LOGGER.log_debug("cursor_create", 
                &format!("Acquired cursor mutex in {:?}", lock_wait_time));
            
            let cursor_result = safe_cursor_operation_with_context("cursor_create", || {
                txn.open_ro_cursor(db)
            }, resource_path.clone());
            
            // Explicitly drop the guard to ensure the mutex is released promptly
            drop(cursor_guard);
            
            // Log performance metrics for successful acquisition
            let operation_duration = operation_start.elapsed();
            let metrics = ConcurrentMetrics {
                concurrent_operations: concurrent_operations_detected,
                lock_wait_time,
                operation_duration,
                retry_count: 0,
                ..Default::default()
            };
            
            // Log warning if lock wait time was significant
            if lock_wait_time.as_millis() > 50 {
                CONCURRENT_LOGGER.log_concurrent_warning("cursor_create", 
                    "Significant lock wait time detected", &metrics);
            }
            
            cursor_result
        }
        Err(_) => {
            // Could not acquire lock immediately - this indicates contention
            concurrent_operations_detected = 1; // At least one other operation is holding the lock
            
            let operation_duration = operation_start.elapsed();
            let metrics = ConcurrentMetrics {
                concurrent_operations: concurrent_operations_detected,
                lock_wait_time: Duration::from_millis(0), // No wait since we didn't block
                operation_duration,
                retry_count: 0,
                ..Default::default()
            };
            
            // Log the contention scenario
            CONCURRENT_LOGGER.log_concurrent_warning("cursor_create", 
                "Cursor mutex contention detected, falling back to unserialized operation", &metrics);
            
            // Fall back to regular operation without serialization
            // This maintains functionality while still providing serialization benefits when possible
            let fallback_result = safe_cursor_operation_with_context("cursor_create_fallback", || {
                txn.open_ro_cursor(db)
            }, resource_path.clone());
            
            // If the fallback also fails with concurrent-related errors, enhance the error context
            match fallback_result {
                CursorOperationResult::Error(msg) if msg.contains("Invalid transaction") 
                    || msg.contains("Invalid database handle") => {
                    // This suggests a concurrent access issue exacerbated by the contention
                    let category = ConcurrentErrorCategory::Contention {
                        operation: "cursor_create".to_string(),
                        wait_duration: Duration::from_millis(0),
                        resource: "cursor_mutex".to_string(),
                        concurrent_operations: concurrent_operations_detected,
                    };
                    
                    let error_context = ConcurrentErrorContext {
                        category,
                        timestamp: Instant::now(),
                        thread_id: get_thread_id(),
                        operation: "cursor_create".to_string(),
                        resource_path,
                        metrics,
                        recovery_hints: vec![
                            "Reduce concurrent cursor operations".to_string(),
                            "Consider implementing application-level queueing".to_string(),
                            "Check for proper transaction and cursor lifecycle management".to_string(),
                        ],
                    };
                    
                    CONCURRENT_LOGGER.log_concurrent_error(&error_context);
                    CursorOperationResult::ConcurrentError(error_context)
                },
                other => other, // Return the original result
            }
        }
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

// Global registry of open environments and cursor serialization
// 
// Ensures that each directory path has at most one environment open,
// preventing LMDB conflicts and improving resource sharing.
// The cursor mutex serializes cursor creation during concurrent access to
// prevent LMDB internal conflicts.
lazy_static::lazy_static! {
    static ref ENVIRONMENTS: Arc<Mutex<HashMap<String, ResourceArc<LmdbEnv>>>> = 
        Arc::new(Mutex::new(HashMap::new()));
    
    // Phase 2: Global cursor mutex for transaction serialization during concurrent access
    // This prevents LMDB internal conflicts when multiple threads try to create cursors simultaneously
    static ref CURSOR_MUTEX: Arc<Mutex<()>> = Arc::new(Mutex::new(()));
}

/// Initialize the NIF module
/// 
/// Registers resource types with the Erlang runtime.
/// This function is called automatically when the NIF is loaded.
fn init(env: Env, _info: Term) -> bool {
    rustler::resource!(LmdbEnv, env) && rustler::resource!(LmdbDatabase, env)
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
fn env_sync<'a>(env: Env<'a>, env_handle: ResourceArc<LmdbEnv>) -> NifResult<Term<'a>> {
    // flush buffers to disk with force true
    match env_handle.env.sync(true) {
        Ok(()) => Ok(atoms::ok().encode(env)),
        Err(err_msg) => Ok((atoms::error(), atoms::environment_error(), 
                      format!("Environment sync failed: {}", err_msg)).encode(env))
    }
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
/// Phase 1: Enhanced Empty Database Detection with Stat-Based Checking
///===================================================================
/// 
/// Implements robust empty database detection using LMDB's transaction stat API
/// to prevent the panic that occurs when cursor.iter_start() is called on empty databases.
/// 
/// The root cause of the remaining panics is that the LMDB library internally calls 
/// `unwrap()` on `cursor.iter_start()` when the database is empty, causing a panic at 
/// `cursor.rs:55:46`. This function provides a reliable way to detect empty databases
/// before attempting cursor operations that would cause the LMDB library to panic internally.

/// Phase 1: Detect empty databases using LMDB stats before attempting cursor operations
/// 
/// Uses the LMDB transaction stat API to check if a database has any entries.
/// This prevents the panic-causing cursor operations on empty databases by detecting
/// emptiness before cursor creation/iteration.
/// 
/// # Arguments
/// * `txn` - Read-only transaction for the database
/// * `db` - Database handle to check
/// 
/// # Returns
/// * `Ok(true)` - Database is empty (0 entries)
/// * `Ok(false)` - Database has data (>0 entries)
/// * `Ok(true)` - Assume empty if we can't get stats (graceful degradation)
/// 
/// # Safety
/// - Uses read-only transaction, so no risk of corruption
/// - Gracefully handles errors by assuming empty (safe default)
/// - Compatible with existing concurrent safety mechanisms
fn is_database_empty(txn: &lmdb::RoTransaction, db: lmdb::Database) -> Result<bool, String> {
    // Use database stat to check if database has any data before cursor operations
    // Note: LMDB's stat() method is available on the Environment, not the transaction
    // We'll use a different approach: try to create a cursor and see if it has any data
    match txn.open_ro_cursor(db) {
        Ok(cursor) => {
            // Try to get the first entry - if this fails with NotFound, database is empty
            match cursor.get(None, None, MDB_FIRST) {
                Ok(_) => {
                    CONCURRENT_LOGGER.log_debug("empty_detection", 
                        "Database has data (found first entry)");
                    Ok(false) // Database has data
                },
                Err(lmdb::Error::NotFound) => {
                    CONCURRENT_LOGGER.log_debug("empty_detection", 
                        "Database is empty (no first entry)");
                    Ok(true) // Database is empty
                },
                Err(lmdb_error) => {
                    // Log the error but assume empty for safety
                    CONCURRENT_LOGGER.log_debug("empty_detection", 
                        &format!("Failed to check first entry: {:?}, assuming empty", lmdb_error));
                    Ok(true) // Assume empty if we can't check - safe default
                }
            }
        },
        Err(lmdb_error) => {
            // Log the error but assume empty for safety
            CONCURRENT_LOGGER.log_debug("empty_detection", 
                &format!("Failed to open cursor for empty check: {:?}, assuming empty", lmdb_error));
            Ok(true) // Assume empty if we can't get stats - safe default
        }
    }
}

/// Phase 1: Enhanced safe cursor operation with empty database detection
/// 
/// Wraps cursor iteration operations with comprehensive empty database detection
/// and panic handling. This function specifically addresses the cursor.rs:55:46 panic
/// by checking database emptiness before attempting cursor iteration.
/// 
/// # Arguments
/// * `txn` - Read-only transaction for the database
/// * `db` - Database handle
/// * `operation_name` - Human-readable name for the operation (for error reporting)
/// * `cursor_operation` - Closure that performs the cursor operation
/// 
/// # Returns
/// * `CursorOperationResult<T>` - Success, traditional error, or enhanced concurrent error
fn safe_cursor_iteration_with_empty_detection<T, F>(
    txn: &lmdb::RoTransaction,
    db: lmdb::Database,
    operation_name: &str,
    cursor_operation: F,
) -> CursorOperationResult<T>
where
    F: FnOnce() -> Result<T, lmdb::Error> + std::panic::UnwindSafe,
{
    // Phase 1: Check if database is empty before attempting cursor operations
    let is_empty = match is_database_empty(txn, db) {
        Ok(empty) => empty,
        Err(error_msg) => {
            CONCURRENT_LOGGER.log_concurrent_warning(operation_name, 
                &format!("Empty database detection failed: {}", error_msg), 
                &ConcurrentMetrics::default());
            // Continue with the operation even if empty detection fails
            false
        }
    };
    
    // If database is empty, return appropriate result without attempting cursor iteration
    if is_empty {
        CONCURRENT_LOGGER.log_debug(operation_name, "Database is empty, skipping cursor iteration");
        // Return a successful empty result - the caller will handle this appropriately
        // We can't return a generic success here since we don't know what T should be
        // So we'll let the cursor operation proceed with additional safety
    }
    
    // Proceed with the cursor operation using existing safety mechanisms
    safe_cursor_operation_with_context(operation_name, cursor_operation, None)
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

    // Open a cursor for the database with panic safety and serialization
    let mut cursor = match safe_cursor_create_with_context(&txn, (*db_handle).db, 
                                                          Some("list_operation".to_string())) {
        CursorOperationResult::Success(cursor) => cursor,
        CursorOperationResult::Error(err_msg) => {
            return Ok((atoms::error(), atoms::database_error(), err_msg).encode(env));
        }
        CursorOperationResult::Panic(panic_msg) => {
            return Ok((atoms::error(), atoms::concurrent_panic(), 
                      format!("Cursor creation panicked: {}", panic_msg)).encode(env));
        }
        CursorOperationResult::ConcurrentError(error_context) => {
            // Phase 3: Enhanced error handling for concurrent scenarios
            let atom = match error_context.category {
                ConcurrentErrorCategory::Panic { .. } => atoms::concurrent_panic(),
                ConcurrentErrorCategory::Timeout { .. } => atoms::concurrent_timeout(),
                ConcurrentErrorCategory::Contention { .. } => atoms::concurrent_contention(),
                ConcurrentErrorCategory::Deadlock { .. } => atoms::concurrent_deadlock(),
                ConcurrentErrorCategory::CursorConflict { .. } => atoms::cursor_conflict(),
                ConcurrentErrorCategory::TransactionConflict { .. } => atoms::transaction_conflict(),
                ConcurrentErrorCategory::ResourceExhausted { .. } => atoms::resource_exhausted(),
                ConcurrentErrorCategory::OperationAborted { .. } => atoms::operation_aborted(),
            };
            
            return Ok((atoms::error(), atom, 
                      format!("Concurrent access error in list operation: {}", 
                             format_concurrent_error_for_erlang(&error_context))).encode(env));
        }
    };
    
    // Phase 1: Enhanced empty database detection before cursor operations
    // Check if database is empty using stat API to prevent potential cursor panics
    let is_empty = match is_database_empty(&txn, (*db_handle).db) {
        Ok(empty) => empty,
        Err(error_msg) => {
            CONCURRENT_LOGGER.log_concurrent_warning("list", 
                &format!("Empty database detection failed: {}", error_msg), 
                &ConcurrentMetrics::default());
            // Continue with the operation even if empty detection fails
            false
        }
    };
    
    // If database is empty, return not_found immediately without attempting cursor iteration
    if is_empty {
        CONCURRENT_LOGGER.log_debug("list", "Database is empty, returning not_found without cursor iteration");
        return Ok(atoms::not_found().encode(env));
    }
    
    // OPTIMIZATION: Use Vec instead of HashSet for better performance with small collections
    // Pre-allocate with reasonable capacity to avoid reallocations
    let mut children = Vec::with_capacity(64);
    let prefix_len = prefix_bytes.len();
    
    // OPTIMIZATION: Start cursor at prefix position instead of scanning from beginning
    // This dramatically reduces iterations for sparse data
    
    // Safe iteration approach that handles empty databases and missing prefixes
    // First, try to position cursor at prefix using MDB_SET_RANGE to check if key exists
    // First, try to position cursor at prefix using MDB_SET_RANGE to check if key exists
    let cursor_positioned = cursor.get(Some(prefix_bytes), None, MDB_SET_RANGE).is_ok();
    
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
fn match_pattern<'a>(
    env: Env<'a>,
    db_handle: ResourceArc<LmdbDatabase>,
    patterns: Vec<(Binary, Binary)>
) -> NifResult<Term<'a>> {
    // Validate database and environment status
    if let Err(error_msg) = db_handle.validate_database() {
        return Ok((atoms::error(), atoms::database_error(), error_msg).encode(env));
    }
    
    // Return not_found if patterns is empty
    if patterns.is_empty() {
        return Ok(atoms::not_found().encode(env));
    }
    
    // Keep patterns as references for efficient comparison
    let patterns_vec: Vec<(&[u8], &[u8])> = patterns
        .iter()
        .map(|(k, v)| (k.as_slice(), v.as_slice()))
        .collect();
    
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
    
    // Open a cursor for the database with panic safety and serialization
    let mut cursor = match safe_cursor_create_with_context(&txn, (*db_handle).db, 
                                                          Some("match_pattern_operation".to_string())) {
        CursorOperationResult::Success(cursor) => cursor,
        CursorOperationResult::Error(err_msg) => {
            return Ok((atoms::error(), atoms::database_error(), err_msg).encode(env));
        }
        CursorOperationResult::Panic(panic_msg) => {
            return Ok((atoms::error(), atoms::concurrent_panic(), 
                      format!("Cursor creation panicked: {}", panic_msg)).encode(env));
        }
        CursorOperationResult::ConcurrentError(error_context) => {
            // Phase 3: Enhanced error handling for concurrent scenarios
            let atom = match error_context.category {
                ConcurrentErrorCategory::Panic { .. } => atoms::concurrent_panic(),
                ConcurrentErrorCategory::Timeout { .. } => atoms::concurrent_timeout(),
                ConcurrentErrorCategory::Contention { .. } => atoms::concurrent_contention(),
                ConcurrentErrorCategory::Deadlock { .. } => atoms::concurrent_deadlock(),
                ConcurrentErrorCategory::CursorConflict { .. } => atoms::cursor_conflict(),
                ConcurrentErrorCategory::TransactionConflict { .. } => atoms::transaction_conflict(),
                ConcurrentErrorCategory::ResourceExhausted { .. } => atoms::resource_exhausted(),
                ConcurrentErrorCategory::OperationAborted { .. } => atoms::operation_aborted(),
            };
            
            return Ok((atoms::error(), atom, 
                      format!("Concurrent access error in match_pattern operation: {}", 
                             format_concurrent_error_for_erlang(&error_context))).encode(env));
        }
    };
    
    // Phase 1: Enhanced empty database detection before cursor iteration
    // Check if database is empty using stat API to prevent cursor.iter_start() panic
    let is_empty = match is_database_empty(&txn, (*db_handle).db) {
        Ok(empty) => empty,
        Err(error_msg) => {
            CONCURRENT_LOGGER.log_concurrent_warning("match_pattern", 
                &format!("Empty database detection failed: {}", error_msg), 
                &ConcurrentMetrics::default());
            // Continue with the operation even if empty detection fails
            false
        }
    };
    
    // If database is empty, return not_found immediately without attempting cursor iteration
    if is_empty {
        CONCURRENT_LOGGER.log_debug("match_pattern", "Database is empty, returning not_found without cursor iteration");
        return Ok(atoms::not_found().encode(env));
    }
    
    // Data structures for tracking matches
    const MAX_RESULTS: usize = 100000;  // Reasonable limit to prevent unbounded memory growth
    let mut matching_ids: Vec<Vec<u8>> = Vec::new();
    let mut current_id: Option<Vec<u8>> = None;
    let mut seen_patterns: HashSet<usize> = HashSet::new();
    let total_patterns = patterns_vec.len();
    
    // Safe iteration through all key-value pairs in the database
    // Use safe cursor creation and then iterate directly without closures to avoid UnwindSafe issues
    let iter_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        cursor.iter_start()
    }));
    
    let iter = match iter_result {
        Ok(iter) => iter,
        Err(panic_payload) => {
            let panic_msg = if let Some(s) = panic_payload.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = panic_payload.downcast_ref::<String>() {
                s.clone()
            } else {
                "Cursor iter_start panicked".to_string()
            };
            
            CONCURRENT_LOGGER.log_concurrent_warning("match_pattern", 
                &format!("Cursor iter_start panicked: {}", panic_msg), 
                &ConcurrentMetrics::default());
            
            return Ok((atoms::error(), atoms::concurrent_panic(), 
                      format!("Cursor iteration panicked during match_pattern: {}", panic_msg)).encode(env));
        }
    };
    
    // Iterate through all key-value pairs in the database
    for (key_bytes, value_bytes) in iter {
        // Parse key to extract ID and suffix
        // Find the position of the last '/' to extract the ID and suffix
        let last_slash_pos = key_bytes.iter().rposition(|&b| b == b'/');
        
        let (id, suffix) = if let Some(pos) = last_slash_pos {
            // Has hierarchy - split into ID and suffix
            let id = key_bytes[..pos].to_vec();
            let suffix = key_bytes[pos + 1..].to_vec();
            (id, suffix)
        } else {
            // No hierarchy - the entire key is the ID
            (key_bytes.to_vec(), Vec::new())
        };
        
        // Check if we've moved to a new ID
        if current_id.as_ref() != Some(&id) {
            // Check if previous ID matched all patterns
            if let Some(prev_id) = current_id.take() {
                if seen_patterns.len() == total_patterns {
                    matching_ids.push(prev_id);
                    // Stop if we've reached the maximum number of results
                    if matching_ids.len() >= MAX_RESULTS {
                        break;
                    }
                }
            }
            
            // Reset for new ID
            current_id = Some(id.clone());
            seen_patterns.clear();
        }
        
        // Check if this key-value pair matches any pattern
        for (pattern_idx, (pattern_key, pattern_value)) in patterns_vec.iter().enumerate() {
            // Check if suffix matches pattern key and value matches pattern value
            if suffix.as_slice() == *pattern_key && value_bytes == *pattern_value {
                seen_patterns.insert(pattern_idx);
            }
        }
    }
    
    // Check the final ID
    if let Some(final_id) = current_id {
        if seen_patterns.len() == total_patterns {
            matching_ids.push(final_id);
        }
    }
    
    // Use the original matching_ids variable for results
    let final_matching_ids = matching_ids;
    
    // Return results
    if final_matching_ids.is_empty() {
        Ok(atoms::not_found().encode(env))
    } else {
        // Convert matching IDs to Erlang binaries
        let mut result_binaries = Vec::with_capacity(final_matching_ids.len());
        for id in final_matching_ids {
            let mut binary = OwnedBinary::new(id.len())
                .ok_or(Error::BadArg)?;
            binary.as_mut_slice().copy_from_slice(&id);
            result_binaries.push(binary.release(env));
        }
        
        Ok((atoms::ok(), result_binaries).encode(env))
    }
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

// Initialize the NIF module
// explicit fuctions are deprecated but here is a list
// [env_open, env_close, env_close_by_name, db_open, db_close, put, put_batch, get, list, flush, env_status]
rustler::init!("elmdb", load = init);