# Makefile for elmdb-rs project

.PHONY: all compile test clean distclean docs examples help

# Default target
all: compile

# Compile the project
compile:
	@echo "Compiling elmdb-rs..."
	rebar3 compile

# Run all tests
test: compile
	@echo "Running all tests..."
	rebar3 ct

# Run specific test suite
test-basic: compile
	@echo "Running basic tests..."
	rebar3 ct --suite test/elmdb_basic_SUITE

test-list: compile
	@echo "Running list operation tests..."
	rebar3 ct --suite test/elmdb_list_SUITE

test-error: compile
	@echo "Running error handling tests..."
	rebar3 ct --suite test/elmdb_error_SUITE

test-perf: compile
	@echo "Running performance tests..."
	rebar3 ct --suite test/elmdb_perf_SUITE

# Run tests with coverage
test-coverage: compile
	@echo "Running tests with coverage..."
	rebar3 ct --cover

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
	@echo "  all         - Compile the project (default)"
	@echo "  compile     - Compile the project"
	@echo "  test        - Run all tests"
	@echo "  test-basic  - Run basic functionality tests"
	@echo "  test-list   - Run list operation tests"
	@echo "  test-error  - Run error handling tests"
	@echo "  test-perf   - Run performance tests"
	@echo "  test-coverage - Run tests with coverage analysis"
	@echo "  examples    - Run example code"
	@echo "  docs        - Generate documentation"
	@echo "  shell       - Start Erlang shell with project loaded"
	@echo "  check       - Run dialyzer for static analysis"
	@echo "  format      - Format source code"
	@echo "  clean       - Clean build artifacts"
	@echo "  distclean   - Clean everything including dependencies"
	@echo "  help        - Show this help message"