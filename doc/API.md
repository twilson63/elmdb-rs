# elmdb-rs API Documentation

This document provides comprehensive API documentation for elmdb-rs, including function specifications, parameters, return values, error codes, and usage examples.

## Table of Contents

- [Environment Management](#environment-management)
- [Database Operations](#database-operations)
- [Key-Value Operations](#key-value-operations)
- [List Operations](#list-operations)
- [Error Handling](#error-handling)
- [Data Types](#data-types)
- [Usage Patterns](#usage-patterns)

## Environment Management

### env_open/2

Opens an LMDB environment for database operations.

```erlang
env_open(Path, Options) -> {ok, Env} | {error, Reason}
```

**Parameters:**

- `Path` :: `binary() | string()`
  - Directory path where database files will be stored
  - Must be an existing directory or a path where directory can be created
  - Examples: `"/var/lib/myapp/db"`, `<<"./data/cache">>`

- `Options` :: `list()`
  - Configuration options for the environment
  - See [Environment Options](#environment-options) below

**Return Values:**

- `{ok, Env}` - Successfully opened environment
  - `Env` :: `term()` - Opaque environment handle for use in other operations
- `{error, already_open}` - Environment already open for this path
- `{error, environment_error}` - Failed to open environment (permissions, disk space, etc.)
- `{error, invalid_path}` - Invalid or inaccessible path

**Environment Options:**

- `{map_size, Size}` :: `{atom(), pos_integer()}`
  - Maximum size of the database in bytes
  - Default: `1073741824` (1GB)
  - Must be multiple of OS page size
  - Can be increased later but not decreased
  - Example: `{map_size, 10737418240}` for 10GB

- `no_mem_init` :: `atom()`
  - Don't initialize malloc'd memory before writing to disk
  - Performance optimization for trusted environments
  - Slightly faster writes but may leak data from memory
  - Example: `no_mem_init`

- `no_sync` :: `atom()`
  - Don't flush system buffers to disk when committing transactions
  - Much faster writes but less durable (data may be lost on system crash)
  - Use for temporary data or when performance is critical
  - Example: `no_sync`

**Usage Examples:**

```erlang
% Basic usage with defaults
{ok, Env} = elmdb:env_open("/path/to/db", []).

% With custom map size
{ok, Env} = elmdb:env_open("/path/to/db", [{map_size, 5368709120}]). % 5GB

% Performance optimized (less durable)
{ok, Env} = elmdb:env_open("/tmp/cache", [no_sync, no_mem_init]).

% Error handling
case elmdb:env_open("/invalid/path", []) of
    {ok, Env} -> 
        ok;
    {error, environment_error} ->
        io:format("Failed to open database~n")
end.
```

### env_close/1

Closes an LMDB environment and releases all associated resources.

```erlang
env_close(Env) -> ok
```

**Parameters:**

- `Env` :: `term()`
  - Environment handle obtained from `env_open/2`

**Return Values:**

- `ok` - Environment closed successfully

**Usage Examples:**

```erlang
{ok, Env} = elmdb:env_open("/path/to/db", []),
% ... perform database operations ...
ok = elmdb:env_close(Env).
```

**Important Notes:**

- All database handles associated with this environment become invalid
- Pending transactions are automatically aborted
- It's safe to call this multiple times on the same handle
- Should be called in cleanup code to prevent resource leaks

### env_close_by_name/1

Closes an environment by its directory path (fallback method).

```erlang
env_close_by_name(Path) -> ok | {error, not_found}
```

**Parameters:**

- `Path` :: `binary() | string()`
  - Directory path of the environment to close

**Return Values:**

- `ok` - Environment closed successfully
- `{error, not_found}` - No environment found for the given path

**Usage Examples:**

```erlang
% Close environment when handle is not available
ok = elmdb:env_close_by_name("/path/to/db").

% Error handling
case elmdb:env_close_by_name("/nonexistent/db") of
    ok -> ok;
    {error, not_found} -> 
        io:format("Environment was not open~n")
end.
```

## Database Operations

### db_open/2

Opens a database within an LMDB environment.

```erlang
db_open(Env, Options) -> {ok, DB} | {error, Reason}
```

**Parameters:**

- `Env` :: `term()`
  - Environment handle from `env_open/2`

- `Options` :: `list()`
  - Configuration options for the database
  - See [Database Options](#database-options) below

**Return Values:**

- `{ok, DB}` - Successfully opened database
  - `DB` :: `term()` - Opaque database handle for key-value operations
- `{error, not_found}` - Database doesn't exist and `create` not specified
- `{error, database_error}` - General database error

**Database Options:**

- `create` :: `atom()`
  - Create the database if it doesn't exist
  - Without this option, `db_open/2` will fail if database doesn't exist
  - Example: `create`

**Usage Examples:**

```erlang
% Open existing database
{ok, DB} = elmdb:db_open(Env, []).

% Create database if it doesn't exist
{ok, DB} = elmdb:db_open(Env, [create]).

% Error handling
case elmdb:db_open(Env, []) of
    {ok, DB} -> 
        % Database opened successfully
        DB;
    {error, not_found} ->
        % Database doesn't exist, create it
        {ok, DB} = elmdb:db_open(Env, [create]),
        DB;
    {error, database_error} ->
        error("Failed to open database")
end.
```

## Key-Value Operations

### put/3

Stores a key-value pair in the database.

```erlang
put(DB, Key, Value) -> ok | {error, Type, Description}
```

**Parameters:**

- `DB` :: `term()`
  - Database handle from `db_open/2`

- `Key` :: `binary()`
  - Key to store (must be binary)
  - Keys are sorted lexicographically
  - Maximum size depends on LMDB configuration (typically 511 bytes)
  - Example: `<<"user/123">>`

- `Value` :: `binary()`
  - Value to store (must be binary)
  - Maximum size depends on available space and map_size
  - Can be empty binary (`<<>>`)
  - Example: `<<"Alice Smith">>`

**Return Values:**

- `ok` - Key-value pair stored successfully
- `{error, key_exist, Description}` - Key already exists (overwrite not attempted)
- `{error, map_full, Description}` - Database is full (increase map_size)
- `{error, txn_full, Description}` - Transaction is full (too many operations)
- `{error, database_error, Description}` - General database error
- `{error, transaction_error, Description}` - Failed to create or commit transaction

**Usage Examples:**

```erlang
% Basic put operation
ok = elmdb:put(DB, <<"user/123">>, <<"Alice">>).

% Store structured data (JSON)
UserData = jiffy:encode(#{name => <<"Alice">>, age => 30}),
ok = elmdb:put(DB, <<"user/123/profile">>, UserData).

% Hierarchical keys
ok = elmdb:put(DB, <<"config/database/host">>, <<"localhost">>),
ok = elmdb:put(DB, <<"config/database/port">>, <<"5432">>).

% Error handling
case elmdb:put(DB, <<"key">>, <<"value">>) of
    ok -> 
        io:format("Stored successfully~n");
    {error, map_full, Desc} ->
        io:format("Database full: ~s~n", [Desc]);
    {error, Type, Desc} ->
        io:format("Error ~p: ~s~n", [Type, Desc])
end.

% Store binary data
BinaryData = term_to_binary({user, 123, <<"Alice">>, [admin, user]}),
ok = elmdb:put(DB, <<"user/123/data">>, BinaryData).
```

### get/2

Retrieves a value by key from the database.

```erlang
get(DB, Key) -> {ok, Value} | not_found | {error, Type, Description}
```

**Parameters:**

- `DB` :: `term()`
  - Database handle from `db_open/2`

- `Key` :: `binary()`
  - Key to retrieve
  - Must be binary, same as used in `put/3`

**Return Values:**

- `{ok, Value}` - Key found, returns associated value
  - `Value` :: `binary()` - The stored value
- `not_found` - Key does not exist in database
- `{error, database_error, Description}` - Database operation error
- `{error, transaction_error, Description}` - Failed to create read transaction

**Usage Examples:**

```erlang
% Basic get operation
{ok, Value} = elmdb:get(DB, <<"user/123">>).

% Handle missing keys
case elmdb:get(DB, <<"user/456">>) of
    {ok, Value} ->
        io:format("Found: ~s~n", [Value]);
    not_found ->
        io:format("User not found~n");
    {error, Type, Desc} ->
        io:format("Error: ~p ~s~n", [Type, Desc])
end.

% Retrieve and decode JSON
case elmdb:get(DB, <<"user/123/profile">>) of
    {ok, JsonBinary} ->
        UserProfile = jiffy:decode(JsonBinary, [return_maps]),
        io:format("User: ~p~n", [UserProfile]);
    not_found ->
        io:format("Profile not found~n")
end.

% Retrieve and decode Erlang terms
case elmdb:get(DB, <<"user/123/data">>) of
    {ok, BinaryData} ->
        {user, UserId, Name, Roles} = binary_to_term(BinaryData),
        io:format("User ~p: ~s with roles ~p~n", [UserId, Name, Roles]);
    not_found ->
        io:format("User data not found~n")
end.
```

## List Operations

### list/2

Lists direct children of a key prefix, useful for hierarchical data structures.

```erlang
list(DB, KeyPrefix) -> {ok, [Child]} | not_found
```

**Parameters:**

- `DB` :: `term()`
  - Database handle from `db_open/2`

- `KeyPrefix` :: `binary()`
  - Key prefix to search for
  - Returns next path components after the prefix
  - Should typically end with a separator (e.g., `/`)

**Return Values:**

- `{ok, Children}` - Found children for the prefix
  - `Children` :: `[binary()]` - List of child components
  - Children are the next path components after the prefix
  - Does not include the separator in results
- `not_found` - No keys found with the given prefix

**Usage Examples:**

```erlang
% Set up hierarchical data
ok = elmdb:put(DB, <<"users/alice/name">>, <<"Alice">>),
ok = elmdb:put(DB, <<"users/alice/email">>, <<"alice@example.com">>),
ok = elmdb:put(DB, <<"users/bob/name">>, <<"Bob">>),
ok = elmdb:put(DB, <<"users/charlie/name">>, <<"Charlie">>).

% List all users
{ok, Users} = elmdb:list(DB, <<"users/">>).
% Users = [<<"alice">>, <<"bob">>, <<"charlie">>]

% List Alice's attributes
{ok, AliceAttrs} = elmdb:list(DB, <<"users/alice/">>).
% AliceAttrs = [<<"name">>, <<"email">>]

% List configuration sections
ok = elmdb:put(DB, <<"config/database/host">>, <<"localhost">>),
ok = elmdb:put(DB, <<"config/server/port">>, <<"8080">>),
ok = elmdb:put(DB, <<"config/cache/ttl">>, <<"3600">>).

{ok, ConfigSections} = elmdb:list(DB, <<"config/">>).
% ConfigSections = [<<"database">>, <<"server">>, <<"cache">>]

{ok, DatabaseConfig} = elmdb:list(DB, <<"config/database/">>).
% DatabaseConfig = [<<"host">>]

% Handle no matches
case elmdb:list(DB, <<"nonexistent/">>) of
    {ok, Children} ->
        io:format("Found children: ~p~n", [Children]);
    not_found ->
        io:format("No children found~n")
end.
```

**Implementation Details:**

The `list/2` operation uses LMDB cursors for efficient prefix scanning:

1. Starts cursor at first key matching prefix
2. Iterates through keys while they match prefix
3. Extracts next path component after prefix
4. Returns unique set of immediate children
5. Stops when keys no longer match prefix (lexicographic ordering)

**Performance Characteristics:**

- Time complexity: O(log n + k) where k is number of matching keys
- Space complexity: O(children) for result set
- Very efficient for tree-like data structures
- Takes advantage of LMDB's sorted key storage

## Error Handling

### Error Types

elmdb-rs provides structured error information for robust error handling:

#### Environment Errors

- `already_open` - Environment already open for path
- `environment_error` - General environment operation failure
- `invalid_path` - Path doesn't exist or not accessible
- `permission_denied` - Insufficient permissions

#### Database Errors

- `not_found` - Database doesn't exist (when opening without `create`)
- `database_error` - General database operation failure

#### Transaction Errors

- `transaction_error` - Failed to begin, commit, or abort transaction
- `key_exist` - Key already exists (in contexts where this is an error)
- `map_full` - Database has reached maximum size
- `txn_full` - Transaction has too many operations

### Error Handling Patterns

```erlang
% Pattern 1: Simple case matching
case elmdb:get(DB, Key) of
    {ok, Value} -> process_value(Value);
    not_found -> default_value();
    {error, Type, Desc} -> handle_error(Type, Desc)
end.

% Pattern 2: Nested operations with error propagation
with_database(Path, Fun) ->
    case elmdb:env_open(Path, []) of
        {ok, Env} ->
            try
                case elmdb:db_open(Env, [create]) of
                    {ok, DB} ->
                        try
                            Fun(DB)
                        catch
                            Class:Reason:Stack ->
                                {error, {operation_failed, Class, Reason, Stack}}
                        end;
                    {error, Reason} ->
                        {error, {db_open_failed, Reason}}
                end
            after
                elmdb:env_close(Env)
            end;
        {error, Reason} ->
            {error, {env_open_failed, Reason}}
    end.

% Pattern 3: Robust transaction-like operations
safe_multi_put(DB, KeyValuePairs) ->
    Results = [elmdb:put(DB, K, V) || {K, V} <- KeyValuePairs],
    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> ok;
        false -> 
            Errors = [R || R <- Results, R =/= ok],
            {error, {partial_failure, Errors}}
    end.
```

## Data Types

### Type Specifications

```erlang
-type env_handle() :: term().
-type db_handle() :: term().
-type key() :: binary().
-type value() :: binary().
-type env_option() :: {map_size, pos_integer()} | no_mem_init | no_sync.
-type db_option() :: create.
-type error_type() :: atom().
-type error_description() :: string().
```

### Key Design Guidelines

1. **Use Binary Keys**: Always use binaries for keys, not strings or atoms
2. **Hierarchical Structure**: Use `/` separator for hierarchy
3. **Lexicographic Ordering**: Design keys to sort naturally
4. **Length Considerations**: Keep keys reasonably short (< 200 bytes recommended)

**Path Separator Convention:**

elmdb-rs uses `/` (forward slash) as the standard path separator for hierarchical keys. This convention:
- Enables efficient prefix-based operations with the `list/2` function
- Provides natural tree-like data organization
- Maintains consistency across the codebase and examples
- Is compatible with filesystem-like key naming patterns

When using hierarchical keys, always use `/` as the separator for optimal performance and consistency with the `list/2` operation implementation.

```erlang
% Good key designs
<<"user/123">>
<<"users/alice/profile/name">>
<<"config/database/host">>
<<"session/abc123/data">>
<<"doc/2024/01/15/report.pdf">>

% Avoid
user_123                    % Atom, not binary
"user/123"                  % String, not binary
<<1,2,3,4>>                % Opaque binary (hard to debug)
```

### Value Encoding

Values must be binaries but can encode various data types:

```erlang
% Raw binary
Value = <<"Alice Smith">>,
ok = elmdb:put(DB, Key, Value).

% JSON encoding
Value = jiffy:encode(#{name => <<"Alice">>, age => 30}),
ok = elmdb:put(DB, Key, Value).

% Erlang term encoding
Value = term_to_binary({user, 123, <<"Alice">>, [admin]}),
ok = elmdb:put(DB, Key, Value).

% Protocol Buffers, MessagePack, etc.
Value = protobuf:encode(UserMsg),
ok = elmdb:put(DB, Key, Value).
```

## Usage Patterns

### Pattern 1: Configuration Store

```erlang
-module(config_store).
-export([start/1, get/1, set/2, list_section/1, stop/0]).

-define(DB_PATH, "/etc/myapp/config").

start(MapSize) ->
    {ok, Env} = elmdb:env_open(?DB_PATH, [{map_size, MapSize}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    put(config_db, {Env, DB}),
    ok.

get(Key) ->
    {_Env, DB} = get(config_db),
    case elmdb:get(DB, Key) of
        {ok, Value} -> {ok, Value};
        not_found -> {error, not_found}
    end.

set(Key, Value) ->
    {_Env, DB} = get(config_db),
    elmdb:put(DB, Key, Value).

list_section(Section) ->
    {_Env, DB} = get(config_db),
    elmdb:list(DB, <<Section/binary, "/">>).

stop() ->
    {Env, _DB} = get(config_db),
    elmdb:env_close(Env),
    erase(config_db).
```

### Pattern 2: Session Store

```erlang
-module(session_store).
-export([create_session/2, get_session/1, delete_session/1]).

create_session(UserId, SessionData) ->
    SessionId = generate_session_id(),
    {ok, DB} = get_database(),
    
    % Store session data
    ok = elmdb:put(DB, <<"sessions/", SessionId/binary>>, 
                   jiffy:encode(SessionData)),
    
    % Index by user
    ok = elmdb:put(DB, <<"user_sessions/", UserId/binary>>, SessionId),
    
    {ok, SessionId}.

get_session(SessionId) ->
    {ok, DB} = get_database(),
    case elmdb:get(DB, <<"sessions/", SessionId/binary>>) of
        {ok, JsonData} ->
            {ok, jiffy:decode(JsonData, [return_maps])};
        not_found ->
            {error, session_not_found}
    end.

delete_session(SessionId) ->
    {ok, DB} = get_database(),
    
    % Get session to find user
    case get_session(SessionId) of
        {ok, #{<<"user_id">> := UserId}} ->
            % Mark session as expired
            ok = elmdb:put(DB, <<"sessions/", SessionId/binary>>, 
                          <<"EXPIRED">>),
            
            % Remove user index
            ok = elmdb:put(DB, <<"user_sessions/", UserId/binary>>, 
                          <<"EXPIRED">>);
        {error, session_not_found} ->
            ok
    end.
```

### Pattern 3: Document Store with Indexing

```erlang
-module(document_store).
-export([store_document/3, get_document/1, find_by_author/1]).

store_document(DocId, Content, Metadata) ->
    {ok, DB} = get_database(),
    
    % Store document content
    ok = elmdb:put(DB, <<"docs/", DocId/binary>>, Content),
    
    % Store metadata
    maps:fold(fun(Key, Value, ok) ->
        MetaKey = <<"meta/", DocId/binary, "/", Key/binary>>,
        ok = elmdb:put(DB, MetaKey, Value)
    end, ok, Metadata),
    
    % Create author index
    case maps:get(<<"author">>, Metadata, undefined) of
        undefined -> ok;
        Author ->
            IndexKey = <<"index/author/", Author/binary, "/", DocId/binary>>,
            ok = elmdb:put(DB, IndexKey, <<"">>)
    end.

get_document(DocId) ->
    {ok, DB} = get_database(),
    case elmdb:get(DB, <<"docs/", DocId/binary>>) of
        {ok, Content} ->
            % Get metadata
            {ok, MetaKeys} = elmdb:list(DB, <<"meta/", DocId/binary, "/">>),
            Metadata = maps:from_list([
                begin
                    MetaKey = <<"meta/", DocId/binary, "/", Key/binary>>,
                    {ok, Value} = elmdb:get(DB, MetaKey),
                    {Key, Value}
                end || Key <- MetaKeys
            ]),
            {ok, Content, Metadata};
        not_found ->
            {error, document_not_found}
    end.

find_by_author(Author) ->
    {ok, DB} = get_database(),
    case elmdb:list(DB, <<"index/author/", Author/binary, "/">>) of
        {ok, DocIds} ->
            Documents = [get_document(DocId) || DocId <- DocIds],
            {ok, [Doc || {ok, _, _} = Doc <- Documents]};
        not_found ->
            {ok, []}
    end.
```

This comprehensive API documentation provides developers with everything needed to effectively use elmdb-rs in their applications.