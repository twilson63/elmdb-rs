# Makefile for elmdb-rs project

.PHONY: all compile test clean distclean docs examples help

# Default target
all: compile

# Compile the project
compile:
	@echo "Compiling elmdb-rs..."
	rebar3 compile

# Run all tests (EUnit) - excludes benchmarks
test: compile
	@echo "Cleaning up leftover LMDB lock files..."
	@rm -rf /tmp/elmdb_test_* 2>/dev/null || true
	@echo "Running all EUnit tests (excluding benchmarks)..."
	@mv test/elmdb_benchmark.erl test/elmdb_benchmark.erl.skip 2>/dev/null || true
	-@rebar3 eunit
	@mv test/elmdb_benchmark.erl.skip test/elmdb_benchmark.erl 2>/dev/null || true

# Run all tests with verbose output - excludes benchmarks
test-verbose: compile
	@echo "Cleaning up leftover LMDB lock files..."
	@rm -rf /tmp/elmdb_test_* 2>/dev/null || true
	@echo "Running all tests with verbose output (excluding benchmarks)..."
	@mv test/elmdb_benchmark.erl test/elmdb_benchmark.erl.skip 2>/dev/null || true
	-@rebar3 eunit -v
	@mv test/elmdb_benchmark.erl.skip test/elmdb_benchmark.erl 2>/dev/null || true

# Run all tests including benchmarks
test-all: compile
	@echo "Cleaning up leftover LMDB lock files..."
	@rm -rf /tmp/elmdb_test_* 2>/dev/null || true
	@echo "Running all tests including benchmarks..."
	rebar3 eunit

# Run only benchmark tests
test-benchmarks: compile
	@echo "Running benchmark tests..."
	rebar3 ct --suite=elmdb_benchmark

# Run benchmarks
benchmark: compile
	@echo "Running benchmarks..."
	rebar3 shell --eval "elmdb_benchmark:run(), halt()."

# Run large-scale benchmarks (1M records)
benchmark-1m: compile
	@echo "Running large-scale benchmark with 1M records..."
	rebar3 shell --eval "elmdb_large_benchmark:run(), halt()."

# Run large-scale benchmarks (10M records)
benchmark-10m: compile
	@echo "Running large-scale benchmark with 10M records..."
	@echo "WARNING: This will take several minutes and use ~10GB of disk space"
	rebar3 shell --eval "elmdb_large_benchmark:run_10m(), halt()."

# Run custom size benchmark (e.g., make benchmark-custom SIZE=5000000)
benchmark-custom: compile
	@echo "Running benchmark with $(SIZE) records..."
	rebar3 shell --eval "elmdb_large_benchmark:run($(SIZE)), halt()."

# Run tests with coverage - excludes benchmarks
test-coverage: compile
	@echo "Cleaning up leftover LMDB lock files..."
	@rm -rf /tmp/elmdb_test_* 2>/dev/null || true
	@echo "Running tests with coverage (excluding benchmarks)..."
	@mv test/elmdb_benchmark.erl test/elmdb_benchmark.erl.skip 2>/dev/null || true
	-@rebar3 eunit --cover
	@mv test/elmdb_benchmark.erl.skip test/elmdb_benchmark.erl 2>/dev/null || true
	rebar3 cover --verbose

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rebar3 clean
	rm -rf _build/
	rm -rf logs/
	rm -rf .rebar3/

# Complete clean including dependencies
distclean: clean
	@echo "Cleaning everything including dependencies..."
	rm -rf _build/
	rm -rf deps/

# Run examples
examples: compile
	@echo "Running examples..."
	erl -pa _build/default/lib/elmdb/ebin -noshell -eval "elmdb_example:run_all_examples(), halt()."

# Generate documentation
docs:
	@echo "Generating documentation..."
	rebar3 edoc

# Start Erlang shell with project loaded
shell: compile
	@echo "Starting Erlang shell..."
	rebar3 shell

# Check syntax and run dialyzer
check: compile
	@echo "Running dialyzer..."
	rebar3 dialyzer

# Format code
format:
	@echo "Formatting code..."
	rebar3 fmt

# Show help
help:
	@echo "Available targets:"
	@echo "  all              - Compile the project (default)"
	@echo "  compile          - Compile the project"
	@echo "  test             - Run unit tests only (excludes benchmarks)"
	@echo "  test-verbose     - Run unit tests with verbose output"
	@echo "  test-all         - Run all tests including benchmarks"
	@echo "  test-benchmarks  - Run only benchmark tests"
	@echo "  test-coverage    - Run unit tests with coverage analysis"
	@echo "  benchmark        - Run quick performance benchmarks"
	@echo "  benchmark-1m     - Run benchmarks with 1 million records"
	@echo "  benchmark-10m    - Run benchmarks with 10 million records"
	@echo "  benchmark-custom - Run benchmarks with custom SIZE=N"
	@echo "  examples         - Run example code"
	@echo "  docs             - Generate documentation"
	@echo "  shell            - Start Erlang shell with project loaded"
	@echo "  check            - Run dialyzer for static analysis"
	@echo "  format           - Format source code"
	@echo "  clean            - Clean build artifacts"
	@echo "  distclean        - Clean everything including dependencies"
	@echo "  help             - Show this help message"