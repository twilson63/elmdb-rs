%%%-------------------------------------------------------------------
%%% @doc
%%% LMDB NIF bindings for Erlang
%%% 
%%% This module provides Erlang bindings for LMDB (Lightning Memory-Mapped Database)
%%% through a Rust NIF implementation.
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb).

%% Environment management
-export([env_open/2, env_close/1, env_close_by_name/1, env_force_close/1, env_force_close_by_name/1, env_status/1]).

%% Database operations
-export([db_open/2, db_close/1, reset/2]).

%% Key-value operations
-export([put/3, put_batch/2, get/2, flush/1]).

%% List operations
-export([list/2]).

%% NIF loading
-export([init/0]).

-on_load(init/0).

%%%===================================================================
%%% NIF Loading
%%%===================================================================

%% @doc Initialize and load the NIF library
-spec init() -> ok | {error, term()}.
init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, elmdb_nif]);
                _ ->
                    filename:join([priv, elmdb_nif])
            end;
        Dir ->
            filename:join(Dir, elmdb_nif)
    end,
    erlang:load_nif(SoName, 0).

%%%===================================================================
%%% Environment Management
%%%===================================================================

%% @doc Create or open an LMDB environment
%% @param Path Directory path for the database files
%% @param Options Configuration options:
%%   - {map_size, integer()}: Maximum database size in bytes
%%   - no_mem_init: Don't initialize malloc'd memory before writing to disk
%%   - no_sync: Don't flush system buffers to disk when committing
%%   - write_map: Use a writeable memory map for better performance
%% @returns {ok, Env} where Env is an opaque environment handle
-spec env_open(Path :: binary() | string(), Options :: list()) -> 
    {ok, term()} | {error, already_open | environment_error}.
env_open(_Path, _Options) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Close an LMDB environment and release resources
%% @param Env Environment handle from env_open
%% @returns ok
-spec env_close(Env :: term()) -> ok | {error, environment_error, binary()}.
env_close(_Env) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Close an environment by its directory path (fallback method)
%% @param Path Directory path of the database
%% @returns ok
-spec env_close_by_name(Path :: binary() | string()) -> ok | {error, environment_error, binary()} | {error, not_found}.
env_close_by_name(_Path) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Force close an environment, even with active database references
%% @param Env Environment handle from env_open
%% @returns ok or {ok, Warning} if there were active references
-spec env_force_close(Env :: term()) -> ok | {ok, string()}.
env_force_close(_Env) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Force close an environment by its directory path, even with active database references
%% @param Path Directory path of the database
%% @returns ok or {ok, Warning} if there were active references
-spec env_force_close_by_name(Path :: binary() | string()) -> ok | {ok, environment_error, binary()}.
env_force_close_by_name(_Path) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Get status information about an environment
%% @param Env Environment handle
%% @returns {ok, Closed, RefCount, Path} where Closed is boolean, RefCount is integer
-spec env_status(Env :: term()) -> {ok, boolean(), integer(), string()}.
env_status(_Env) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Database Operations
%%%===================================================================

%% @doc Open a database within an environment
%% @param Env Environment handle
%% @param Options Configuration options:
%%   - create: Create the database if it doesn't exist
%% @returns {ok, DBInstance} where DBInstance is an opaque database handle
-spec db_open(Env :: term(), Options :: list()) -> 
    {ok, term()} | {error, not_found} | {error, database_error, binary()}.
db_open(_Env, _Options) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Close a database handle and decrement environment reference count
%% @param DBInstance Database handle from db_open
%% @returns ok
-spec db_close(DBInstance :: term()) -> ok | {error, database_error, binary()}.
db_close(_DBInstance) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Key-Value Operations
%%%===================================================================

%% @doc Write a key-value pair to the database
%% @param DBInstance Database handle
%% @param Key The key to write (binary)
%% @param Value The value to store (binary)
%% @returns ok on success
%% @throws {error, Type, Description} on failure
-spec put(DBInstance :: term(), Key :: binary(), Value :: binary()) -> 
    ok | {error, database_error | transaction_error, binary()}.
put(_DBInstance, _Key, _Value) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Write multiple key-value pairs to the database in a single transaction
%% @param DBInstance Database handle
%% @param KeyValuePairs List of {Key, Value} tuples where Key and Value are binaries
%% @returns ok on success, or {ok, SuccessCount, Errors} if some writes failed
%% @throws {error, Type, Description} on failure
-spec put_batch(DBInstance :: term(), KeyValuePairs :: [{binary(), binary()}]) -> 
    ok | {ok, integer(), list()} | {error, database_error | transaction_error, binary()}.
put_batch(_DBInstance, _KeyValuePairs) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Read a value by key from the database
%% @param DBInstance Database handle
%% @param Key The key to read (binary)
%% @returns {ok, Value} where Value is a binary, or {error, not_found} if key doesn't exist
-spec get(DBInstance :: term(), Key :: binary()) -> 
    {ok, binary()} | {error, not_found} | {error, database_error | transaction_error, binary()}.
get(_DBInstance, _Key) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% List Operations
%%%===================================================================

%% @doc List all direct children of a group using prefix matching
%% @param DBInstance Database handle
%% @param Key The key prefix to search for (binary)
%% @returns {ok, Children} where Children is a list of binaries, or {error, not_found}
-spec list(DBInstance :: term(), Key :: binary()) -> 
    {ok, binary()} | {error, not_found} | {error, database_error | transaction_error, binary()}.
list(_DBInstance, _Key) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Explicitly flush any buffered writes to disk
%% @param DBInstance Database handle
%% @returns ok on success
-spec flush(DBInstance :: term()) -> ok | {error, database_error | transaction_error, binary()}.
flush(_DBInstance) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Reset Operations
%%%===================================================================

%% @doc Reset a database by closing it, removing files, and recreating it fresh
%% @param Path Directory path for the database files
%% @param Options Configuration options (same as env_open)
%% @returns {ok, Env, DBInstance} where Env is environment handle and DBInstance is database handle
-spec reset(Path :: binary() | string(), Options :: list()) -> 
    {ok, term(), term()} | {error, term()}.
reset(_Path, _Options) ->
    erlang:nif_error(nif_not_loaded).