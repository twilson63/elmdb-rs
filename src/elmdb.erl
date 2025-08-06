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
-export([env_open/2, env_close/1, env_close_by_name/1, env_status/1]).

%% Database operations
-export([db_open/2, db_close/1]).

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
    PrivDir = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true -> filename:join(["..", priv]);
                _ -> priv
            end;
        Dir ->
            Dir
    end,
    % Try different library names based on OS
    LibNames = case os:type() of
        {unix, darwin} -> ["libelmdb_nif", "elmdb_nif"];
        {unix, _} -> ["libelmdb_nif", "elmdb_nif"];
        _ -> ["elmdb_nif"]
    end,
    load_nif_from_list(PrivDir, LibNames).

load_nif_from_list(_PrivDir, []) ->
    {error, {load_failed, "Failed to load NIF library: no suitable library found"}};
load_nif_from_list(PrivDir, [LibName | Rest]) ->
    SoName = filename:join([PrivDir, LibName]),
    case erlang:load_nif(SoName, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, _Reason} -> 
            load_nif_from_list(PrivDir, Rest)
    end.

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
%%          {error, directory_not_found} if the directory doesn't exist
%%          {error, permission_denied} if lacking permissions
%%          {error, already_open} if environment is already open
%%          {error, no_space} if disk is full
%%          {error, corrupted} if database is corrupted
%%          {error, ErrorAtom} for other errors
-spec env_open(Path :: binary() | string(), Options :: list()) -> 
    {ok, term()} | {error, term()}.
env_open(_Path, _Options) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Close an LMDB environment and release resources
%% @param Env Environment handle from env_open
%% @returns ok
-spec env_close(Env :: term()) -> ok.
env_close(_Env) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Close an environment by its directory path (fallback method)
%% @param Path Directory path of the database
%% @returns ok
-spec env_close_by_name(Path :: binary() | string()) -> ok.
env_close_by_name(_Path) ->
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
    {ok, term()} | {error, term()}.
db_open(_Env, _Options) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Close a database handle and decrement environment reference count
%% @param DBInstance Database handle from db_open
%% @returns ok
-spec db_close(DBInstance :: term()) -> ok | {error, term(), string()}.
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
    ok | {error, term(), binary()}.
put(_DBInstance, _Key, _Value) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Write multiple key-value pairs to the database in a single transaction
%% @param DBInstance Database handle
%% @param KeyValuePairs List of {Key, Value} tuples where Key and Value are binaries
%% @returns ok on success, or {ok, SuccessCount, Errors} if some writes failed
%% @throws {error, Type, Description} on failure
-spec put_batch(DBInstance :: term(), KeyValuePairs :: [{binary(), binary()}]) -> 
    ok | {ok, integer(), list()} | {error, term(), binary()}.
put_batch(_DBInstance, _KeyValuePairs) ->
    erlang:nif_error(nif_not_loaded).
 
%% @doc Read a value by key from the database
%% @param DBInstance Database handle
%% @param Key The key to read (binary)
%% @returns {ok, Value} where Value is a binary, or not_found if key doesn't exist
-spec get(DBInstance :: term(), Key :: binary()) -> 
    {ok, binary()} | not_found.
get(_DBInstance, _Key) ->
    erlang:nif_error(nif_not_loaded).


%%%===================================================================
%%% List Operations
%%%===================================================================

%% @doc List all direct children of a group using prefix matching
%% @param DBInstance Database handle
%% @param Key The key prefix to search for (binary)
%% @returns {ok, Children} where Children is a list of binaries, or not_found
-spec list(DBInstance :: term(), Key :: binary()) -> 
    {ok, [binary()]} | not_found.
list(_DBInstance, _Key) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Explicitly flush any buffered writes to disk
%% @param DBInstance Database handle
%% @returns ok on success
-spec flush(DBInstance :: term()) -> ok | {error, term(), binary()}.
flush(_DBInstance) ->
    erlang:nif_error(nif_not_loaded).
