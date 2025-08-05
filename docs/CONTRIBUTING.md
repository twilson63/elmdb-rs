# Contributing to elmdb-rs

Thank you for your interest in contributing to elmdb-rs! This document provides guidelines and information for developers who want to contribute to the project.

## Table of Contents

- [Development Environment Setup](#development-environment-setup)
- [Building the Project](#building-the-project)
- [Running Tests](#running-tests)
- [Performance Benchmarking](#performance-benchmarking)
- [Code Style and Standards](#code-style-and-standards)
- [Submitting Changes](#submitting-changes)
- [Debugging](#debugging)
- [Project Architecture](#project-architecture)

## Development Environment Setup

### Prerequisites

**Required Tools:**
- Erlang/OTP 24+ (recommended: 26+ for best compatibility)
- Rust 1.70+ (recommended: 1.75+ for optimal performance)
- Git
- Make (optional but recommended)
- rebar3 3.20+ 

**Platform-Specific Setup:**

**Linux (Ubuntu/Debian):**
```bash
# Install system dependencies
sudo apt-get update
sudo apt-get install build-essential git pkg-config

# Install Erlang (via Erlang Solutions)
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update
sudo apt-get install esl-erlang rebar3

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env
```

**macOS:**
```bash
# Install Homebrew dependencies
brew install erlang rebar3 rust git

# Or use asdf for version management
brew install asdf
asdf plugin add erlang
asdf plugin add rust
asdf install erlang 26.2.5
asdf install rust 1.75.0
asdf global erlang 26.2.5
asdf global rust 1.75.0
```

### Project Setup

```bash
# Clone and setup
git clone https://github.com/your-org/elmdb-rs.git
cd elmdb-rs

# Install rebar3 plugins
rebar3 plugins install rebar3_cargo

# Build the project
make

# Verify setup by running tests
make test
```

### Development Tools

**Recommended IDE Setup:**
- **VS Code** with extensions:
  - Erlang LS
  - rust-analyzer
  - GitLens
- **Emacs** with erlang-mode and rust-mode
- **IntelliJ IDEA** with Erlang and Rust plugins

**Useful Development Commands:**
```bash
# Start development shell
make shell

# Format code
make format

# Run static analysis
make check

# Generate documentation
make docs
```

## Building the Project

### Standard Build

```bash
# Full clean build
make distclean
make

# Development build (faster, with debug symbols)
cd native/elmdb_nif
cargo build
cd ../..
rebar3 compile
```

### Build Configurations

**Debug Build (development):**
```bash
cd native/elmdb_nif
cargo build --profile dev
cd ../..
rebar3 compile
```

**Release Build (production):**
```bash
cd native/elmdb_nif
cargo build --release
cd ../..
rebar3 compile
```

**Optimized Build (benchmarking):**
```bash
cd native/elmdb_nif
RUSTFLAGS="-C target-cpu=native" cargo build --release
cd ../..
rebar3 compile
```

### Cross-Compilation

**For Linux on macOS:**
```bash
# Install cross-compilation target
rustup target add x86_64-unknown-linux-gnu

# Install cross-compilation toolchain (macOS)
brew install SergioBenitez/osxct/x86_64-unknown-linux-gnu

# Build
cd native/elmdb_nif
cargo build --release --target x86_64-unknown-linux-gnu
```

## Running Tests

### Test Suite Overview

The project includes several test suites:

| Test Suite | Purpose | Scope | Runtime |
|-----------|---------|-------|---------|
| `elmdb_basic_SUITE` | Core API functionality | Unit tests | ~2s |
| `elmdb_list_SUITE` | Hierarchical operations | Integration tests | ~3s |
| `elmdb_error_SUITE` | Error handling | Edge cases | ~2s |
| `elmdb_perf_SUITE` | Basic performance | Performance tests | ~15s |
| `elmdb_final_perf_SUITE` | Optimized performance | Benchmarks | ~10s |

### Running Tests

**All tests:**
```bash
make test
# or
rebar3 ct
```

**Specific test suites:**
```bash
# Core functionality
make test-basic
# or
rebar3 ct --suite test/elmdb_basic_SUITE

# List operations
make test-list

# Error handling
make test-error

# Performance tests
make test-perf
```

**Test with coverage:**
```bash
make test-coverage
# or
rebar3 ct --cover

# View coverage report
open _build/test/cover/index.html
```

### Test Configuration

**Environment Variables:**
```bash
# Custom test database location
export ELMDB_TEST_DIR="/tmp/elmdb_dev_tests"

# Performance test configuration
export ELMDB_PERF_RECORDS=25000      # Reduce for faster tests
export ELMDB_TEST_TIMEOUT=60000      # Test timeout in milliseconds
export ELMDB_SKIP_PERF_TESTS=true    # Skip performance tests

# Run with custom configuration
rebar3 ct
```

**Test Database Cleanup:**
```bash
# Clean test databases
rm -rf /tmp/elmdb_*

# Or use automatic cleanup (in test code)
# Tests should clean up automatically, but manual cleanup may be needed during development
```

### Writing Tests

**Basic Test Structure:**
```erlang
%% test/my_feature_SUITE.erl
-module(my_feature_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([test_my_feature/1]).

all() -> [test_my_feature].

init_per_suite(Config) ->
    % Setup for entire suite
    Config.

end_per_suite(_Config) ->
    % Cleanup for entire suite
    ok.

init_per_testcase(_TestCase, Config) ->
    % Setup before each test
    TestDir = "/tmp/elmdb_test_" ++ atom_to_list(?MODULE),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    % Cleanup after each test
    TestDir = ?config(test_dir, Config),
    os:cmd("rm -rf " ++ TestDir),
    ok.

test_my_feature(Config) ->
    TestDir = ?config(test_dir, Config),
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 104857600}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test your feature here
    ok = elmdb:put(DB, <<"key">>, <<"value">>),
    {ok, <<"value">>} = elmdb:get(DB, <<"key">>),
    
    % Cleanup
    ok = elmdb:env_close(Env).
```

## Performance Benchmarking

### Running Benchmarks

**Quick Benchmark:**
```bash
make test-perf
```

**Detailed Benchmark:**
```bash
# Run optimized performance tests
rebar3 ct --suite test/elmdb_final_perf_SUITE

# Or use the benchmark script
erl -pa _build/default/lib/elmdb/ebin -noshell -eval "
{ok, _} = benchmark_optimized:run_benchmark(),
halt()."
```

### Custom Benchmarks

**Create benchmark script:**
```erlang
%% benchmark_custom.erl
-module(benchmark_custom).
-export([run/0]).

run() ->
    {ok, Env} = elmdb:env_open("/tmp/benchmark_db", [
        {map_size, 2147483648},  % 2GB
        no_sync,                 % Performance optimization
        no_mem_init,            % Performance optimization
        write_map               % Performance optimization
    ]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Benchmark writes
    Records = 100000,
    StartTime = erlang:system_time(microsecond),
    
    lists:foreach(fun(I) ->
        Key = <<"benchmark/key/", (integer_to_binary(I))/binary>>,
        Value = <<"value_", (integer_to_binary(I))/binary, "_data">>,
        ok = elmdb:put(DB, Key, Value)
    end, lists:seq(1, Records)),
    
    WriteTime = erlang:system_time(microsecond) - StartTime,
    WriteRate = round(Records * 1000000 / WriteTime),
    
    io:format("Wrote ~p records in ~p microseconds (~p records/sec)~n",
              [Records, WriteTime, WriteRate]),
    
    % Benchmark reads
    ReadStartTime = erlang:system_time(microsecond),
    
    lists:foreach(fun(I) ->
        Key = <<"benchmark/key/", (integer_to_binary(I))/binary>>,
        {ok, _Value} = elmdb:get(DB, Key)
    end, lists:seq(1, Records)),
    
    ReadTime = erlang:system_time(microsecond) - ReadStartTime,
    ReadRate = round(Records * 1000000 / ReadTime),
    
    io:format("Read ~p records in ~p microseconds (~p records/sec)~n",
              [Records, ReadTime, ReadRate]),
    
    ok = elmdb:env_close(Env).
```

**Run custom benchmark:**
```bash
erl -pa _build/default/lib/elmdb/ebin
> c(benchmark_custom).
> benchmark_custom:run().
```

### Performance Profiling

**Using Erlang profiler:**
```erlang
% Start profiler
eprof:start().
eprof:start_profiling([self()]).

% Run your code
{ok, Env} = elmdb:env_open("/tmp/profile_db", []),
% ... your operations ...

% Stop and analyze
eprof:stop_profiling().
eprof:analyze().
```

**Using fprof for detailed analysis:**
```erlang
% Profile to file
fprof:trace(start).
% ... your operations ...
fprof:trace(stop).
fprof:profile().
fprof:analyse([{dest, "profile_results.txt"}]).
```

### Memory Profiling

```bash
# Run with memory profiling
erl +Meamin -pa _build/default/lib/elmdb/ebin
> instrument:allocations().
% ... run your code ...
> instrument:allocations().
```

## Code Style and Standards

### Erlang Code Style

**Follow OTP conventions:**
- Use `snake_case` for functions and variables
- Use `CamelCase` for modules and record names
- Indent with 4 spaces (no tabs)
- Maximum line length: 100 characters
- Always use pattern matching over if/case when possible

**Example:**
```erlang
-module(elmdb_helper).
-export([format_key/2, validate_options/1]).

format_key(Prefix, Suffix) when is_binary(Prefix), is_binary(Suffix) ->
    <<Prefix/binary, "/", Suffix/binary>>;
format_key(Prefix, Suffix) ->
    error({badarg, {Prefix, Suffix}}).

validate_options([]) -> 
    [];
validate_options([{map_size, Size} | Rest]) when is_integer(Size), Size > 0 ->
    [{map_size, Size} | validate_options(Rest)];
validate_options([create | Rest]) ->
    [create | validate_options(Rest)];
validate_options([InvalidOption | _]) ->
    error({invalid_option, InvalidOption}).
```

### Rust Code Style

**Follow standard Rust conventions:**
- Use `rustfmt` for formatting
- Use `clippy` for linting
- Follow Rust naming conventions
- Add comprehensive documentation
- Use `Result<T, E>` for error handling

**Setup formatting:**
```bash
cd native/elmdb_nif
rustup component add rustfmt clippy

# Format code
cargo fmt

# Run linting
cargo clippy
```

**Example:**
```rust
use rustler::{Env, Term, NifResult, Error};

/// Opens an LMDB environment with the specified options.
///
/// # Arguments
/// * `env` - The Rustler environment
/// * `path` - Database path as a string
/// * `options` - List of options for environment configuration
///
/// # Returns
/// * `Ok(resource)` - Environment resource handle
/// * `Err(error)` - Error with description
#[rustler::nif]
pub fn env_open(env: Env, path: String, options: Vec<Term>) -> NifResult<ResourceArc<EnvResource>> {
    let parsed_options = parse_env_options(env, options)?;
    
    let lmdb_env = lmdb::Environment::new()
        .set_map_size(parsed_options.map_size)
        .open(&path)
        .map_err(|e| Error::Term(Box::new(format!("Failed to open environment: {}", e))))?;
    
    let resource = ResourceArc::new(EnvResource {
        env: Arc::new(lmdb_env),
        path: path.clone(),
    });
    
    Ok(resource)
}
```

### Documentation Standards

**Erlang documentation (EDoc):**
```erlang
%% @doc Opens an LMDB environment at the specified path.
%%
%% @param Path The filesystem path where the database will be stored
%% @param Options List of configuration options:
%%   - `{map_size, Size}': Maximum database size in bytes
%%   - `no_sync': Don't flush system buffers on commit
%%   - `create': Create database if it doesn't exist
%% @returns `{ok, Env}' on success, `{error, Reason}' on failure
%%
%% @example
%% ```
%% {ok, Env} = elmdb:env_open("/path/to/db", [{map_size, 1073741824}]),
%% '''
-spec env_open(Path :: binary() | string(), Options :: [option()]) -> 
    {ok, env_handle()} | {error, term()}.
env_open(Path, Options) ->
    % Implementation here
```

## Submitting Changes

### Development Workflow

1. **Fork and Clone:**
   ```bash
   git clone https://github.com/your-username/elmdb-rs.git
   cd elmdb-rs
   git remote add upstream https://github.com/your-org/elmdb-rs.git
   ```

2. **Create Feature Branch:**
   ```bash
   git checkout -b feature/my-new-feature
   ```

3. **Make Changes:**
   - Write code following style guidelines
   - Add comprehensive tests
   - Update documentation
   - Ensure all tests pass

4. **Test Thoroughly:**
   ```bash
   make test
   make test-coverage
   make check  # Run dialyzer
   ```

5. **Commit Changes:**
   ```bash
   git add .
   git commit -m "Add feature: brief description
   
   - Detailed change description
   - Why this change is needed
   - Any breaking changes
   
   Fixes #123"
   ```

6. **Push and Create PR:**
   ```bash
   git push origin feature/my-new-feature
   # Create pull request on GitHub
   ```

### Pull Request Guidelines

**PR Description should include:**
- Clear description of the change
- Motivation and context
- Testing performed
- Any breaking changes
- Screenshots (if UI changes)

**Before submitting:**
- [ ] All tests pass
- [ ] Code coverage maintained or improved
- [ ] Documentation updated
- [ ] CHANGELOG.md updated (if applicable)
- [ ] No compiler warnings
- [ ] Dialyzer passes

**Example PR template:**
```markdown
## Summary
Brief description of the change

## Motivation
Why is this change needed?

## Changes
- List of specific changes made
- Any new APIs or behavior changes

## Testing
- How was this tested?
- Any specific test cases added?

## Breaking Changes
- List any breaking changes
- Migration guide if needed

## Checklist
- [ ] Tests pass
- [ ] Documentation updated
- [ ] CHANGELOG updated
```

## Debugging

### Debugging Erlang Code

**Using debugger:**
```bash
# Start with debugger
erl -pa _build/default/lib/elmdb/ebin
> debugger:start().
> int:i(elmdb).  % Interpret module
% Set breakpoints and run code
```

**Debug prints:**
```erlang
% Use io:format for debugging
io:format("Debug: Key=~p, Value=~p~n", [Key, Value]).

% Or use ct:print in tests
ct:print("Test state: ~p", [State]).
```

### Debugging Rust NIF

**Enable debug logging:**
```rust
// In Cargo.toml
[dependencies]
log = "0.4"
env_logger = "0.10"

// In lib.rs
use log::{debug, info, warn, error};

#[rustler::nif]
pub fn debug_function(env: Env, input: Term) -> NifResult<Term> {
    debug!("Function called with input: {:?}", input);
    // ... function body ...
    Ok(input)
}
```

**Set log level:**
```bash
export RUST_LOG=debug
rebar3 shell
```

**Using GDB (Linux/macOS):**
```bash
# Build with debug symbols
cd native/elmdb_nif
cargo build

# Start Erlang under GDB
gdb --args erl -pa _build/default/lib/elmdb/ebin
(gdb) run
# In Erlang shell, trigger the problematic code
# When crash occurs, use GDB commands
(gdb) bt
(gdb) info registers
```

### Common Issues

**NIF crashes:**
- Check for memory leaks in Rust code
- Ensure proper resource management
- Validate all inputs from Erlang

**Performance issues:**
- Profile with `eprof` or `fprof`
- Check LMDB configuration
- Monitor memory usage

**Build issues:**
- Clean build artifacts: `make distclean`
- Check Rust/Erlang versions
- Verify all dependencies installed

## Project Architecture

### Directory Structure

```
elmdb-rs/
├── native/elmdb_nif/          # Rust NIF implementation
│   ├── src/lib.rs            # Main NIF module
│   ├── Cargo.toml            # Rust dependencies
│   └── target/               # Rust build artifacts
├── src/                      # Erlang source code
│   ├── elmdb.erl            # Main API module
│   └── elmdb.app.src        # Application specification
├── test/                     # Test suites
│   ├── elmdb_basic_SUITE.erl # Basic functionality tests
│   ├── elmdb_perf_SUITE.erl  # Performance tests
│   └── ...
├── examples/                 # Usage examples
├── doc/                     # Documentation
├── priv/                    # NIF binaries (generated)
├── rebar.config             # Erlang build configuration
├── Makefile                 # Build automation
└── README.md               # Main documentation
```

### Code Organization

**Erlang Layer (`src/elmdb.erl`):**
- Public API functions
- Input validation
- Error handling and formatting
- Resource management

**Rust Layer (`native/elmdb_nif/src/lib.rs`):**
- LMDB operations
- Memory management
- Thread safety
- Performance optimizations

### Adding New Features

1. **Define API in Erlang:**
   ```erlang
   -spec new_operation(Handle, Key, Options) -> Result.
   new_operation(Handle, Key, Options) ->
       validate_inputs(Handle, Key, Options),
       elmdb_nif:new_operation(Handle, Key, Options).
   ```

2. **Implement in Rust:**
   ```rust
   #[rustler::nif]
   pub fn new_operation(env: Env, handle: ResourceArc<DbResource>, 
                       key: Binary, options: Vec<Term>) -> NifResult<Term> {
       // Implementation
   }
   ```

3. **Add tests:**
   ```erlang
   test_new_operation(_Config) ->
       % Test implementation
   ```

4. **Update documentation:**
   - Add to README.md
   - Update API documentation
   - Add examples

### Release Process

1. **Update version numbers:**
   - `native/elmdb_nif/Cargo.toml`
   - `src/elmdb.app.src`

2. **Update CHANGELOG.md**

3. **Tag release:**
   ```bash
   git tag v0.2.0
   git push origin v0.2.0
   ```

4. **Create GitHub release with binaries**

---

## Getting Help

- **Issues**: Report bugs and request features on GitHub Issues
- **Discussions**: Ask questions in GitHub Discussions
- **Code Review**: All changes go through pull request review
- **Documentation**: Check `/doc` directory and inline documentation

Thank you for contributing to elmdb-rs!