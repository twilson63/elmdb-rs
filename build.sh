#!/bin/bash

# Build script for elmdb-rs
set -e

echo "Building elmdb-rs LMDB NIF..."

# Create priv directory if it doesn't exist
mkdir -p priv

# Build the Rust NIF
echo "Building Rust NIF..."
cd native/elmdb_nif
cargo build --release
cd ../..

# Copy the built library to priv directory
echo "Copying NIF library..."
case "$OSTYPE" in
  darwin*)
    cp native/elmdb_nif/target/release/libelm*.dylib priv/ 2>/dev/null || \
    cp native/elmdb_nif/target/release/libelm*.so priv/ 2>/dev/null || true
    ;;
  linux*)
    cp native/elmdb_nif/target/release/libelm*.so priv/
    ;;
  msys*|cygwin*)
    cp native/elmdb_nif/target/release/elm*.dll priv/
    ;;
  *)
    echo "Unknown OS type: $OSTYPE"
    exit 1
    ;;
esac

# Build the Erlang code
echo "Building Erlang code..."
rebar3 compile

echo "Build completed successfully!"
echo ""
echo "To test the NIF loading:"
echo "  rebar3 shell"
echo "  1> elmdb:init()."