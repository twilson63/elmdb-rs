# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

elmdb-rs is a high-performance Erlang NIF (Native Implemented Function) binding for LMDB (Lightning Memory-Mapped Database) written in Rust. It provides fast, embedded key-value storage for Erlang applications through a Rust implementation that ensures memory safety and crash resistance.

## Build Commands

### Full Build
```bash
make                    # Compile everything (Rust NIF + Erlang)
rebar3 compile         # Alternative: compile with rebar3
```

### Rust NIF Only
```bash
cd native/elmdb_nif
cargo build --release   # Production build
cargo build            # Development build (faster, with debug symbols)
```

### Clean Build
```bash
make distclean         # Remove all build artifacts
make clean             # Remove Erlang build artifacts only
```

## Testing Commands

### Run All Tests
```bash
make test              # Run all EUnit tests
make test-verbose      # Run all tests with verbose output
rebar3 eunit          # Alternative: run with rebar3
```

### Run Specific Test Suites
```bash
make test-basic        # Core functionality tests
make test-list         # List operation tests  
make test-error        # Error handling tests
make test-perf         # Performance benchmarks
make test-fast         # All tests except performance (quick)
rebar3 eunit -m elmdb_basic_tests   # Run specific module directly
```

### Test Coverage
```bash
make test-coverage     # Run tests with coverage analysis
rebar3 eunit --cover  # Alternative with rebar3
```

### Legacy Common Test Suites
```bash
make test-ct          # Run old Common Test suites if needed
rebar3 ct             # Direct Common Test execution
```

### Interactive Test Running
From the Erlang shell:
```erlang
elmdb_tests:run_all().      % Run all tests
elmdb_tests:run_basic().    % Run basic tests only
elmdb_tests:run_list().     % Run list tests only
elmdb_tests:run_error().    % Run error tests only
elmdb_tests:run_perf().     % Run performance tests only
elmdb_tests:run_fast().     % Run all except performance
```

## Development Commands

### Interactive Shell
```bash
make shell             # Start Erlang shell with project loaded
rebar3 shell          # Alternative
```

### Code Quality
```bash
make check            # Run dialyzer for static analysis
make format           # Format code (if configured)
```

### Lint and Typecheck
Since this is a Rust NIF project, also run:
```bash
cd native/elmdb_nif && cargo clippy    # Rust linter
cd native/elmdb_nif && cargo check     # Type checking
```

## Architecture Overview

The project has a two-layer architecture:

1. **Rust NIF Layer** (`native/elmdb_nif/src/lib.rs`)
   - Manages LMDB environments and databases through a global registry
   - Implements write buffering for batch optimization
   - Handles all memory management and safety
   - Provides thread-safe access through Arc/Mutex

2. **Erlang Interface** (`src/elmdb.erl`)
   - Provides idiomatic Erlang API
   - Handles type specifications and documentation
   - All functions return structured error tuples or atoms

### Key Architectural Decisions

1. **Global Environment Registry**: Environments are stored in a global HashMap to ensure single instance per path and proper cleanup.

2. **Reference Counting**: Databases increment/decrement environment reference counts to prevent premature closing.

3. **Write Buffering**: Writes are buffered (default 1000 operations) and flushed as a batch for better performance. Flushes occur:
   - When buffer is full
   - Before any read operation
   - On explicit flush() call
   - When database is closed

4. **Resource Management**: Uses Rust's ResourceArc for automatic cleanup when Erlang garbage collects references.

5. **List Operation Optimization**: The list/2 function uses cursor positioning and early termination for efficient prefix scanning.

## Important Implementation Details

### Error Handling Pattern
All operations return either:
- `ok` or `{ok, Result}` for success
- `not_found` for missing keys
- `{error, Type, Description}` for errors

### Path Conventions
- Uses `/` as the standard path separator for hierarchical keys
- Example: `<<"users/alice/profile/name">>`

### Memory Safety
- All LMDB operations are wrapped in Rust error handling
- No raw pointers exposed to Erlang
- Automatic resource cleanup via Drop traits

### Transaction Model
- Currently uses implicit transactions (one per operation)
- Write buffering provides pseudo-batch behavior
- Future versions may expose explicit transaction API

## Common Development Tasks

### Adding a New NIF Function

1. Add Rust implementation in `native/elmdb_nif/src/lib.rs`
2. Add to `rustler::init!` macro at the bottom of lib.rs
3. Add Erlang wrapper in `src/elmdb.erl` with proper specs
4. Add tests in appropriate test suite
5. Update documentation

### Debugging

1. Check Rust compilation errors:
   ```bash
   cd native/elmdb_nif && cargo build
   ```

2. Enable debug logging in Rust (add println! or use env_logger)

3. Run specific test with console output:
   ```bash
   rebar3 ct --suite test/elmdb_basic_SUITE --case test_put_get
   ```

### Performance Testing

The codebase includes comprehensive benchmarks in:
- `test/elmdb_perf_SUITE.erl` - Standard performance tests
- `test/elmdb_final_perf_SUITE.erl` - Optimized performance tests
- `benchmarks/` - Standalone benchmark scripts

## Project Structure

```
elmdb-rs/
├── src/                    # Erlang source files
│   └── elmdb.erl          # Main Erlang API module
├── native/                 # Rust NIF implementation
│   └── elmdb_nif/         
│       └── src/lib.rs     # Core Rust implementation
├── test/                   # Common Test suites
├── test_scripts/          # Standalone test scripts
├── benchmarks/            # Performance benchmarks
├── docs/                  # Documentation
│   └── internal/          # Internal development docs
└── priv/                  # Compiled NIF libraries
```

## Key Functions

### Environment Management
- `env_open/2` - Opens LMDB environment
- `env_close/1` - Closes environment (checks references)
- `env_close_by_name/1` - Closes environment by path

### Database Operations
- `db_open/2` - Opens database within environment
- `db_close/1` - Closes database and decrements ref count

### Key-Value Operations
- `put/3` - Store key-value pair (buffered)
- `get/2` - Retrieve value by key (flushes buffer first)
- `list/2` - List direct children of prefix (optimized cursor scanning)
- `flush/1` - Explicitly flush write buffer

## Testing Strategy

Tests use EUnit framework and are organized by functionality:
- `elmdb_basic_tests` - Core operations (put/get/batch)
- `elmdb_list_tests` - Hierarchical data and prefix operations
- `elmdb_error_tests` - Error handling and edge cases
- `elmdb_perf_tests` - Performance benchmarks (can be skipped with ELMDB_SKIP_PERF_TESTS=true)

Test organization:
- `test/elmdb_tests.erl` - Master test runner
- `test/eunit/` - EUnit test modules
- `test/` - Legacy Common Test suites (being phased out)

Each test creates isolated temporary directories in `/tmp/elmdb_eunit_tests/` and cleans up after itself.