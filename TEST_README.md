# elmdb-rs Test Suite

This directory contains a comprehensive test suite for the elmdb NIF implementation.

## Test Structure

### Test Suites

1. **`test/elmdb_basic_SUITE.erl`** - Basic functionality tests
   - Environment open/close operations
   - Database open with and without create flag
   - Basic put/get operations
   - Error handling for missing keys
   - Binary key/value handling
   - Large value operations

2. **`test/elmdb_list_SUITE.erl`** - List operations tests
   - Hierarchical key structure testing
   - Listing direct children only
   - Empty results (not_found) handling
   - Prefix matching edge cases
   - Binary prefix handling
   - Deep hierarchy navigation

3. **`test/elmdb_error_SUITE.erl`** - Error handling tests
   - Invalid paths for env_open
   - Double close operations
   - Operations on closed environments/databases
   - Invalid binary inputs
   - Permission denied scenarios
   - Memory exhaustion simulation

4. **`test/elmdb_perf_SUITE.erl`** - Performance/stress tests
   - Bulk put/get operations
   - Large value handling performance
   - Concurrent access patterns
   - Memory usage patterns
   - Sequential vs random access
   - Database growth characteristics

### Examples

**`examples/elmdb_example.erl`** - Comprehensive usage examples
- Basic key-value operations
- Hierarchical data organization
- User session store implementation
- Configuration management
- Document storage with metadata
- Cache implementation with TTL
- File system-like operations
- Batch operations
- Error handling patterns

## Running Tests

### Prerequisites

Ensure you have:
- Erlang/OTP (version 24+ recommended)
- Rebar3
- Rust toolchain (for NIF compilation)

### Quick Start

```bash
# Compile and run all tests
make test

# Run specific test suites
make test-basic
make test-list
make test-error
make test-perf

# Run tests with coverage
make test-coverage
```

### Manual Test Execution

```bash
# Compile first
rebar3 compile

# Run all tests
rebar3 ct

# Run specific suite
rebar3 ct --suite test/elmdb_basic_SUITE

# Run specific test case
rebar3 ct --suite test/elmdb_basic_SUITE --case test_basic_put_get
```

### Running Examples

```bash
# Run all examples
make examples

# Or manually
erl -pa _build/default/lib/elmdb/ebin -noshell -eval "elmdb_example:run_all_examples(), halt()."
```

## Test Configuration

Tests use temporary directories for isolation:
- Each test case gets its own directory under `priv_dir`
- Directories are automatically cleaned up after each test
- Test timeouts are configured appropriately (30s for basic, 10min for performance)

## Performance Test Guidelines

Performance tests are designed to:
- Measure operations per second for various workloads
- Test with different data sizes and access patterns
- Evaluate concurrent access behavior
- Monitor memory usage patterns

Expected performance characteristics:
- Basic put/get operations: >10,000 ops/sec
- Bulk operations: >5,000 ops/sec
- Large values (1MB): <100ms per operation
- Concurrent reads: Should scale with available cores

## Error Handling Tests

Error tests verify graceful handling of:
- Invalid input parameters
- Resource exhaustion
- Permission issues
- Concurrent access violations
- State management errors

## Test Data Organization

Tests use consistent naming patterns:
- Keys: `<<"test_key">>`, `<<"key_000001">>`, etc.
- Hierarchical keys: `<<"users/alice/profile">>`
- Test values: Predictable patterns for verification

## Debugging Tests

Enable verbose output:
```bash
rebar3 ct --verbose

# Or with specific log level
rebar3 ct --logdir ./test_logs --verbosity 100
```

View test results:
- HTML reports: `_build/test/logs/index.html`
- Coverage reports: `_build/test/cover/index.html`

## Contributing Test Cases

When adding new tests:

1. Follow existing naming conventions
2. Use proper cleanup in `end_per_testcase/2`
3. Include both positive and negative test cases
4. Add performance benchmarks for new operations
5. Document expected behavior in test descriptions

### Test Case Template

```erlang
test_new_feature(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Test setup
    TestData = generate_test_data(),
    
    % Execute test
    Result = elmdb:new_operation(DB, TestData),
    
    % Verify results
    expected_result = Result,
    
    % Cleanup
    ok = elmdb:env_close(Env),
    ok.
```

## Integration with CI/CD

Tests are designed to run in automated environments:
- No interactive components
- Configurable timeouts
- Comprehensive error reporting
- Clean exit codes

Example CI configuration:
```bash
#!/bin/bash
set -e

# Compile
rebar3 compile

# Run tests with coverage
rebar3 ct --cover

# Check coverage threshold
rebar3 cover --verbose --min_coverage=80
```