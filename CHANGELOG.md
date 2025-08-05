# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Comprehensive documentation in `docs/` directory
- NIF build documentation
- Configuration system with `config/sys.config`
- Utility scripts in `scripts/` directory
- `.gitkeep` for preserving directory structure

### Changed
- Consolidated ~400 test files into focused test suite with 24 tests
- Reorganized project structure following Erlang/OTP conventions
- Enhanced rebar.config with profiles and better platform support
- Improved README with detailed usage examples and configuration guide
- Updated Makefile to reflect current test structure

### Fixed
- List function now properly returns `not_found` for non-existent prefixes
- NIF loading on macOS (supports both .so and .dylib extensions)
- Cursor iteration using proper `iter_from` method instead of `iter`

## [0.1.0] - 2024-01-01

### Added
- Initial release
- Basic LMDB operations (put, get, list)
- Environment and database management
- Hierarchical key support with efficient prefix operations
- Write batching for improved performance
- Comprehensive test suite
- Performance benchmarks