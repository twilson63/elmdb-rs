# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

elmdb-rs is a high-performance Erlang NIF (Native Implemented Function) binding for LMDB (Lightning Memory-Mapped Database) written in Rust. It provides fast, embedded key-value storage for Erlang/Elixir applications through a Rust implementation that ensures memory safety and crash resistance.

## Build Commands

```bash
# Full build (recommended)
make                    # Compile everything (Rust NIF + Erlang)

# Alternative build
rebar3 compile         # Compile with rebar3 directly

# Clean builds
make clean             # Remove Erlang build artifacts only
make distclean         # Remove all build artifacts

# Rust NIF only (for debugging Rust issues)
cd native/elmdb_nif && cargo build --release   # Production build
cd native/elmdb_nif && cargo build            # Development build
```

## Test Commands

```bash
# Run all tests
make test              # Run all EUnit tests (includes automatic cleanup of leftover LMDB lock files)
make test-verbose      # Run tests with verbose output (includes cleanup)
rebar3 eunit          # Alternative: run with rebar3

# Run specific test types
rebar3 eunit --module=elmdb_test     # Run specific test module
rebar3 eunit --module=elmdb_benchmark # Run benchmarks

# Test coverage
make test-coverage     # Run tests with coverage analysis (includes cleanup)

# Performance testing
make benchmark         # Run performance benchmarks from shell

# Manual cleanup (if needed)
rm -rf /tmp/elmdb_test_*  # Remove leftover LMDB lock files
```

## Development Commands

```bash
# Interactive shell
make shell             # Start Erlang shell with project loaded
rebar3 shell          # Alternative shell

# Code quality checks
make check            # Run dialyzer for static analysis
cd native/elmdb_nif && cargo clippy    # Rust linter
cd native/elmdb_nif && cargo check     # Rust type checking

# Documentation
make docs             # Generate Erlang documentation
```

## Architecture Overview

The project follows a two-layer architecture:

### 1. Rust NIF Layer (`native/elmdb_nif/src/lib.rs`)
- Manages LMDB environments and databases through a global registry
- Uses Arc/Mutex for thread-safe access
- Implements write buffering for batch optimization (default 1000 operations)
- Handles all memory management through Rust's ownership system
- Uses rustler 0.36.2 for Erlang interop

### 2. Erlang Interface (`src/elmdb.erl`)
- Provides idiomatic Erlang API
- All functions return structured error tuples: `{ok, Result}`, `not_found`, or `{error, Type, Description}`
- Handles type specifications and documentation

## Key Implementation Details

### Resource Management
- Environments are stored in a global HashMap to ensure single instance per path
- Databases increment/decrement environment reference counts
- Automatic cleanup via Rust's Drop traits and ResourceArc

### Write Buffering
Writes are buffered for performance and flushed when:
- Buffer reaches 1000 operations
- Before any read operation
- On explicit flush() call  
- When database is closed

### Path Conventions
- Uses `/` as the standard path separator for hierarchical keys
- Example: `<<"users/alice/profile/name">>`

## Core API Functions

### Environment Management
- `env_open/2` - Opens LMDB environment with options like `{map_size, Size}`
- `env_close/1` - Closes environment (checks references)
- `env_close_by_name/1` - Closes environment by path (fallback)

### Database Operations
- `db_open/2` - Opens database within environment
- `db_close/1` - Closes database and decrements ref count

### Key-Value Operations
- `put/3` - Store key-value pair (buffered write)
- `get/2` - Retrieve value by key (flushes buffer first)
- `list/2` - List direct children of prefix (optimized cursor scanning)
- `match/2` - Pattern matching across hierarchical data with multiple key-value patterns
- `flush/1` - Explicitly flush write buffer

### Pattern Matching Operations
- `match/2` - Find entities where ALL specified patterns match (with cursor optimization)
- `match_pattern/2` - Internal NIF function for pattern matching (called by match/2)

## Testing Approach

Tests use EUnit and Common Test frameworks with the following structure:
- `test/elmdb_test.erl` - Consolidated test suite covering basic operations, batch operations, list operations, pattern matching, error handling, and performance
- `test/elmdb_benchmark.erl` - Comprehensive performance benchmarking suite with match feature validation

### Match Feature Benchmarks
The benchmark suite includes extensive match operation testing:

**Core Benchmarks:**
- Single pattern matching with different selectivities
- Multi-pattern scaling (2, 5, 10, 20 patterns)
- Scalability testing (1K to 100K records)
- Hierarchical key performance (2-20 depth levels)

**Advanced Scenarios:**
- Concurrent match operations (1-50 workers)
- Selectivity impact analysis (0.1% to 80% match rates)
- Memory usage profiling

**Cursor Optimization Validation:**
- Optimization effectiveness comparison
- Prefix detection performance
- Early termination benefits
- Validates 50-80% performance improvement targets

Pattern matching tests include:
- Basic single and multi-pattern matching
- Hierarchical data matching scenarios
- Edge cases (empty patterns, non-hierarchical keys)
- Performance testing with large datasets
- Concurrent access patterns

Each test creates isolated temporary directories and cleans up after itself.

## Common Development Tasks

### Adding a New NIF Function
1. Add Rust implementation in `native/elmdb_nif/src/lib.rs`
2. Add to `rustler::init!` macro at the bottom of lib.rs
3. Add Erlang wrapper in `src/elmdb.erl` with proper specs
4. Add tests in `test/elmdb_test.erl`
5. Update documentation in README.md

### Pattern Matching Implementation Notes
The match function uses hierarchical key parsing with cursor optimization for efficient querying:
- **Three-phase optimization**: Pattern analysis, smart cursor positioning, and early termination
- **Conservative prefix detection**: Automatically detects hierarchical patterns (e.g., "users/", "items/")
- **Intelligent cursor positioning**: Uses `cursor.iter_from(prefix)` to skip irrelevant database sections
- **Early termination**: Stops iteration when moving beyond prefix boundaries (leverages LMDB's sorted keys)
- **Safe fallback**: Always falls back to full scan if optimization fails or isn't beneficial
- Keys are split at the last `/` to separate entity ID from field name
- Patterns match against field suffixes and exact values
- Only entities matching ALL patterns are returned
- **Performance**: Sub-millisecond query times on large hierarchical datasets
- **Panic safety**: Graceful handling of empty database edge cases
- **Concurrent safety**: Thread-safe operations with panic isolation and cursor serialization
- Automatically flushes write buffer before reading to ensure consistency

### Concurrency Guidelines
- **Safe concurrent access**: Multiple processes can safely perform match operations simultaneously
- **Panic isolation**: LMDB library panics are caught and converted to proper error tuples
- **Cursor serialization**: Automatic serialization prevents LMDB internal conflicts during cursor creation
- **Error recovery**: Comprehensive error handling with structured logging and recovery hints
- **Performance under load**: Maintains excellent performance (< 5ms) with 50+ concurrent workers
- **Resource management**: Graceful degradation under resource pressure with proper cleanup

### Debugging Issues
```bash
# Check Rust compilation
cd native/elmdb_nif && cargo build

# Run specific test
rebar3 eunit --module=elmdb_test

# Check for NIF loading issues
ls -la priv/  # Should show elmdb_nif.so or equivalent
```

### Performance Considerations
- Default map_size is 1GB, adjust based on needs
- Use `no_sync` option for non-critical data (faster writes)
- Write buffering improves bulk insert performance significantly
- Hierarchical key design enables efficient prefix operations
- **Cursor optimization**: Match operations optimized for hierarchical data patterns
  - Automatic prefix detection and cursor positioning
  - Early termination for bounded queries
  - 50-80% performance improvement on typical workloads
  - Safe fallback ensures no performance regressions
- **Concurrent safety**: Enhanced thread safety for multi-process access
  - Comprehensive panic isolation prevents BEAM VM crashes
  - Transaction serialization for cursor creation prevents LMDB conflicts
  - Structured error handling and logging for concurrent scenarios
  - Supports 50+ concurrent workers with 0% panic rate
  - Graceful degradation under resource pressure

## Dependencies

### Erlang/OTP
- Requires OTP 24+ (tested with 24, 25, 26, 27)
- Uses rebar3 as build tool with rebar3_cargo plugin

### Rust
- Requires Rust 1.70+ with Cargo
- Key dependencies:
  - rustler 0.36.2 (Erlang NIF bindings)
  - lmdb 0.8 (LMDB Rust bindings)
  - lazy_static 1.4 (Global state management)