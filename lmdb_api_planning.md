# LMDB API Planning Document

This document outlines all LMDB (elmdb) API calls used in `hb_store_lmdb.erl` to enable alternative implementations of the storage backend.

This nif should use lmdb-rkb-sys crate and implement the thinnest possible layer, but leverage rust to build out the list operation, by doing partial key set_range queries using cursors.

## Technology Overview

This is an erlang nif that uses rust and lmdb-rs

## 1. Environment Management

### elmdb:env_open/2
**Purpose**: Create or open an LMDB environment (database instance)  
**Location**: src/hb_store_lmdb.erl:57  
**Inputs**:
- `Path` (string): Directory path for the database files
- `Options` (list): Configuration options
  - `{map_size, integer()}`: Maximum database size in bytes
  - `no_mem_init`: Don't initialize malloc'd memory before writing to disk
  - `no_sync`: Don't flush system buffers to disk when committing

**Output**: `{ok, Env}` where `Env` is an opaque environment handle  
**Error**: `{error, Reason}`

### elmdb:env_close/1
**Purpose**: Close an LMDB environment and release resources  
**Location**: src/hb_store_lmdb.erl:626  
**Input**: `Env` - Environment handle from `env_open`  
**Output**: `ok`  
**Error**: Throws error on failure

### elmdb:env_close_by_name/1
**Purpose**: Close an environment by its directory path (fallback method)  
**Location**: src/hb_store_lmdb.erl:634  
**Input**: `Path` (string) - Directory path of the database  
**Output**: `ok`  
**Error**: Throws error on failure

## 2. Database Operations

### elmdb:db_open/2
**Purpose**: Open a database within an environment  
**Location**: src/hb_store_lmdb.erl:64  
**Inputs**:
- `Env`: Environment handle
- `Options` (list): 
  - `create`: Create the database if it doesn't exist

**Output**: `{ok, DBInstance}` where `DBInstance` is an opaque database handle  
**Error**: `{error, Reason}`

## 3. Write Operations

### elmdb:put/3
**Purpose**: write a key-value pair to the database  
**Location**: src/hb_store_lmdb.erl:127  
**Inputs**:
- `DBInstance`: Database handle
- `Key` (binary): The key to write
- `Value` (binary): The value to store

**Output**: `ok` on success  
**Error**: `{error, Type, Description}` where:
- `Type`: Error type atom
- `Description`: Human-readable error description

**Note**: This is an asynchronous operation that doesn't wait for disk commit

## 4. Read Operations

### elmdb:get/2
**Purpose**: Read a value by key from the database  
**Location**: src/hb_store_lmdb.erl:223  
**Inputs**:
- `DBInstance`: Database handle
- `Key` (binary): The key to read

**Output**: 
- `{ok, Value}` where `Value` is a binary
- `not_found` if key doesn't exist

**Error**: May throw exceptions on database errors

## 5. List Operations

### elmdb:list/2
#### `list(DBInstance, Key) -> {ok, Children} | not_found`
**Purpose**: List all direct children of a group.
**Inputs**:
- `DBInstance`: Database handle
- `Key` (binary): The key to read
**Output**:
```erlang
{ok, Children} = elmdb:list(DBInstance, <<"key-path">>).
% Children = [<<"database">>, <<"server">>, <<"logging">>]
```

## Implementation Notes

### Key Design Patterns

1. **Singleton Environment**: Each database directory gets one environment handle stored in `persistent_term`

2. **Writes**: All writes use `put` for performance 

3. **Read-Only Transactions**: All reads and iterations use read-only transactions for consistency

4. **List Operations**: The `list` operation uses cursors with `set_range` to efficiently find keys with a given prefix

5. **Binary Keys/Values**: All keys and values must be binaries

### Required Capabilities for Alternative Implementations

1. **Environment Management**
   - Open/close database environment with size limits
   - Support for no-sync mode (deferred disk writes)

2. **Key-Value Operations**
   - Synchronous put (write)
   - Synchronous get (read)
   - Handle not_found for missing keys

3. **Ordered Key Storage**
   - Keys must be stored in lexicographic order
   - Support range queries (keys >= prefix)

4. **Transactions**
   - Read-only transactions for consistent snapshots
   - Cursor support within transactions

### Error Handling

The implementation must handle:
- `not_found` for missing keys (not an error condition)
- `{error, Type, Description}` for write failures
- Transaction/cursor errors via exceptions

### Performance Considerations

1. writes 
2. Read-only transactions should be lightweight
3. List Opts should be efficient for prefix searches
4. The implementation batches writes for efficiency
