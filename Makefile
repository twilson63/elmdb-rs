# Makefile for elmdb-rs project

.PHONY: all compile test clean distclean docs examples help

# Default target
all: compile

# Compile the project
compile:
	@echo "Compiling elmdb-rs..."
	rebar3 compile

# Run all tests (EUnit)
test: compile
	@echo "Cleaning up leftover LMDB lock files..."
	@rm -rf /tmp/elmdb_test_* 2>/dev/null || true
	@echo "Running all EUnit tests..."
	rebar3 eunit

# Run all tests with verbose output
test-verbose: compile
	@echo "Cleaning up leftover LMDB lock files..."
	@rm -rf /tmp/elmdb_test_* 2>/dev/null || true
	@echo "Running all tests with verbose output..."
	rebar3 eunit -v

# Run benchmarks
benchmark: compile
	@echo "Running benchmarks..."
	rebar3 shell --eval "elmdb_benchmark:run(), halt()."

# Run tests with coverage
test-coverage: compile
	@echo "Cleaning up leftover LMDB lock files..."
	@rm -rf /tmp/elmdb_test_* 2>/dev/null || true
	@echo "Running tests with coverage..."
	rebar3 eunit --cover
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
	@echo "  all           - Compile the project (default)"
	@echo "  compile       - Compile the project"
	@echo "  test          - Run all EUnit tests (with automatic lock file cleanup)"
	@echo "  test-verbose  - Run all tests with verbose output (with cleanup)"
	@echo "  test-coverage - Run tests with coverage analysis (with cleanup)"
	@echo "  benchmark     - Run performance benchmarks"
	@echo "  examples      - Run example code"
	@echo "  docs          - Generate documentation"
	@echo "  shell         - Start Erlang shell with project loaded"
	@echo "  check         - Run dialyzer for static analysis"
	@echo "  format        - Format source code"
	@echo "  clean         - Clean build artifacts"
	@echo "  distclean     - Clean everything including dependencies"
	@echo "  help          - Show this help message"