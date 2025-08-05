# NIF Build Documentation

This document describes the build process for the elmdb-rs Native Implemented Function (NIF) library.

## Overview

elmdb-rs uses Rust to implement a NIF that provides LMDB functionality to Erlang/Elixir applications. The build process involves:

1. Compiling Rust code to a dynamic library
2. Linking against LMDB
3. Making the library available to the Erlang runtime

## Build System Architecture

### Components

- **Rebar3**: Erlang build tool that orchestrates the build
- **rebar3_cargo**: Plugin that integrates Cargo (Rust build tool) with Rebar3
- **Cargo**: Rust package manager and build tool
- **Rustler**: Rust library for creating Erlang NIFs

### Build Flow

```
rebar3 compile
  ├── Pre-hook: mkdir -p priv/
  ├── Cargo build (via rebar3_cargo)
  │   ├── Compile Rust dependencies
  │   ├── Compile elmdb_nif
  │   └── Link with LMDB
  ├── Compile Erlang modules
  └── Post-hook: Copy NIF library to priv/
```

## Configuration

### rebar.config

The main build configuration is in `rebar.config`:

```erlang
%% Cargo plugin configuration
{plugins, [
    {rebar3_cargo, "0.1.1"}
]}.

%% Cargo options
{cargo_opts, [
    {src_dir, "native/elmdb_nif"},
    {cargo_args, ["--release"]}
]}.

%% Build hooks
{provider_hooks, [
    {pre, [{compile, {cargo, build}}]},
    {post, [{clean, {cargo, clean}}]}
]}.

%% Platform-specific library copying
{post_hooks, [
    {"(linux|darwin|solaris|freebsd)", compile, 
     "sh -c 'find native/elmdb_nif/target -name \"libelmdb_nif.*\" -type f ! -path \"*/deps/*\" | head -1 | xargs -I {} cp {} priv/'"},
    {"win32", compile, 
     "powershell -Command \"Copy-Item -Path 'native\\elmdb_nif\\target\\release\\elmdb_nif.dll' -Destination 'priv\\' -Force\""}
]}.
```

### Cargo.toml

The Rust build configuration is in `native/elmdb_nif/Cargo.toml`:

```toml
[package]
name = "elmdb_nif"
version = "0.1.0"
edition = "2021"

[lib]
name = "elmdb_nif"
crate-type = ["cdylib"]

[dependencies]
rustler = "0.29"
lmdb = "0.8"
lazy_static = "1.4"
```

## Platform-Specific Considerations

### Linux

- Library extension: `.so`
- LMDB typically available via package manager
- May need to install development headers

### macOS

- Library extension: `.dylib`
- Erlang expects `.so`, so we create symlinks
- LMDB available via Homebrew

### Windows

- Library extension: `.dll`
- Requires Visual Studio Build Tools
- LMDB compiled automatically by Cargo

## Build Profiles

### Development Build

Fast compilation, includes debug symbols:

```bash
# Using rebar3 (default)
rebar3 compile

# Direct cargo build
cd native/elmdb_nif
cargo build
```

### Release Build

Optimized for performance:

```bash
# Using rebar3
rebar3 as prod compile

# Direct cargo build
cd native/elmdb_nif
cargo build --release
```

### CPU-Specific Optimizations

For maximum performance on deployment hardware:

```bash
cd native/elmdb_nif
RUSTFLAGS="-C target-cpu=native" cargo build --release
```

## Troubleshooting

### Common Build Issues

#### 1. Rust/Cargo Not Found

**Error**: `cargo: command not found`

**Solution**:
```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env
```

#### 2. LMDB Headers Missing

**Error**: `lmdb.h: No such file or directory`

**Solution**:
```bash
# Ubuntu/Debian
sudo apt-get install liblmdb-dev

# macOS
brew install lmdb

# The Rust lmdb crate includes LMDB source, so this is rarely needed
```

#### 3. Library Not Found at Runtime

**Error**: `Failed to load NIF library`

**Solution**:
- Check that the library exists in `priv/`
- Verify library has correct name (elmdb_nif.so or libelmdb_nif.so)
- Check library architecture matches Erlang VM

#### 4. Version Mismatch

**Error**: `NIF library version mismatch`

**Solution**:
- Clean and rebuild: `rebar3 clean && rebar3 compile`
- Ensure Erlang version hasn't changed

#### 5. Permission Issues

**Error**: `Permission denied` during build

**Solution**:
- Don't run build as root
- Ensure write permissions in project directory
- Check that `priv/` directory is writable

### Debug Build Issues

Enable verbose output:

```bash
# Verbose rebar3 build
DEBUG=1 rebar3 compile

# Verbose cargo build
cd native/elmdb_nif
cargo build --verbose
```

Check build artifacts:

```bash
# List built libraries
find . -name "*.so" -o -name "*.dylib" -o -name "*.dll" | grep -v deps

# Check library dependencies (Linux/macOS)
ldd priv/elmdb_nif.so  # Linux
otool -L priv/libelmdb_nif.dylib  # macOS
```

## Advanced Build Options

### Cross-Compilation

For building on one platform for another:

```bash
# Example: Build for Linux x86_64 from macOS
rustup target add x86_64-unknown-linux-gnu
cd native/elmdb_nif
cargo build --target x86_64-unknown-linux-gnu --release
```

### Static Linking

To reduce runtime dependencies:

```toml
# In Cargo.toml
[dependencies]
lmdb = { version = "0.8", features = ["static"] }
```

### Custom LMDB Build

To use a specific LMDB version:

```toml
# In Cargo.toml
[dependencies]
lmdb-sys = { version = "0.8", features = ["static"] }
```

## Continuous Integration

### GitHub Actions Example

```yaml
name: Build
on: [push, pull_request]

jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        otp: [24, 25, 26]
        rust: [stable]
    
    runs-on: ${{ matrix.os }}
    
    steps:
      - uses: actions/checkout@v3
      
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: ${{ matrix.rust }}
          
      - run: rebar3 compile
      - run: rebar3 eunit
```

## Performance Optimization

### Compiler Flags

For production builds, consider these optimizations:

```toml
# In Cargo.toml
[profile.release]
lto = true  # Link-time optimization
codegen-units = 1  # Better optimization
strip = true  # Remove symbols
opt-level = 3  # Maximum optimization
```

### CPU Features

Enable specific CPU features:

```bash
# Intel AVX2
RUSTFLAGS="-C target-feature=+avx2" cargo build --release

# ARM NEON
RUSTFLAGS="-C target-feature=+neon" cargo build --release
```

## References

- [Rustler Documentation](https://github.com/rusterlium/rustler)
- [LMDB Documentation](http://www.lmdb.tech/doc/)
- [Rebar3 Documentation](https://rebar3.org/docs/)
- [rebar3_cargo Plugin](https://github.com/rusterlium/rebar3_cargo)