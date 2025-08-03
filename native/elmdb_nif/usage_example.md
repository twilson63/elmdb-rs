# LMDB NIF Usage Example

This document shows how the implemented LMDB operations should work from Erlang.

## Opening Environment and Database

```erlang
% Open LMDB environment
{ok, Env} = elmdb:env_open(<<"/path/to/db">>, [{map_size, 1073741824}]).

% Open database with create flag
{ok, DB} = elmdb:db_open(Env, [create]).
```

## Key-Value Operations

```erlang
% Put a key-value pair
ok = elmdb:put(DB, <<"mykey">>, <<"myvalue">>).

% Get a value by key
{ok, <<"myvalue">>} = elmdb:get(DB, <<"mykey">>).

% Key not found
not_found = elmdb:get(DB, <<"nonexistent">>).
```

## List Operations (Directory-like)

```erlang
% Put some hierarchical keys
ok = elmdb:put(DB, <<"users/john/name">>, <<"John Doe">>).
ok = elmdb:put(DB, <<"users/jane/name">>, <<"Jane Smith">>).
ok = elmdb:put(DB, <<"users/bob/age">>, <<"25">>).

% List direct children under "users/"
{ok, [<<"john">>, <<"jane">>, <<"bob">>]} = elmdb:list(DB, <<"users/">>).

% List children under "users/john/"
{ok, [<<"name">>]} = elmdb:list(DB, <<"users/john/">>).

% No children found
not_found = elmdb:list(DB, <<"nonexistent/">>).
```

## Cleanup

```erlang
% Close environment
ok = elmdb:env_close(Env).
```

## Error Handling

The implementation provides detailed error information:

```erlang
% Various error cases
{error, transaction_error, "Failed to begin write transaction"} = elmdb:put(DB, Key, Value).
{error, key_exist, "Key already exists"} = elmdb:put(DB, ExistingKey, Value).
{error, map_full, "Database is full"} = elmdb:put(DB, Key, LargeValue).
{error, database_error, "Failed to get value"} = elmdb:get(DB, Key).
```

## Implementation Notes

1. **Transactions**: All operations are properly wrapped in LMDB transactions
2. **Binary Safety**: All keys and values are handled as Erlang binaries
3. **Error Handling**: Comprehensive error reporting with descriptive messages
4. **Path-like Keys**: The list operation treats keys as paths separated by '/'
5. **Resource Management**: LMDB handles are managed as Erlang resources with proper cleanup