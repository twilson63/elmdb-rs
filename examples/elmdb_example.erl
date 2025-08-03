%%%-------------------------------------------------------------------
%%% @doc
%%% Example usage patterns for elmdb NIF
%%% Demonstrates common use cases and best practices
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_example).

-export([
    basic_usage/0,
    hierarchical_data/0,
    user_session_store/0,
    configuration_management/0,
    document_storage/0,
    key_value_cache/0,
    file_system_like_operations/0,
    batch_operations/0,
    error_handling_example/0
]).

%%%===================================================================
%%% Basic Usage Example
%%%===================================================================

%% @doc Basic put/get operations
basic_usage() ->
    io:format("=== Basic Usage Example ===~n"),
    
    % Open environment and database
    {ok, Env} = elmdb:env_open("/tmp/elmdb_basic_example", []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Store some key-value pairs
    ok = elmdb:put(DB, <<"name">>, <<"Alice">>),
    ok = elmdb:put(DB, <<"age">>, <<"30">>),
    ok = elmdb:put(DB, <<"city">>, <<"New York">>),
    
    % Retrieve values
    {ok, Name} = elmdb:get(DB, <<"name">>),
    {ok, Age} = elmdb:get(DB, <<"age">>),
    {ok, City} = elmdb:get(DB, <<"city">>),
    
    io:format("Name: ~s, Age: ~s, City: ~s~n", [Name, Age, City]),
    
    % Try to get a non-existent key
    not_found = elmdb:get(DB, <<"missing_key">>),
    io:format("Missing key returned: not_found~n"),
    
    % Clean up
    ok = elmdb:env_close(Env),
    file:del_dir_r("/tmp/elmdb_basic_example"),
    ok.

%%%===================================================================
%%% Hierarchical Data Example
%%%===================================================================

%% @doc Demonstrate hierarchical key organization
hierarchical_data() ->
    io:format("=== Hierarchical Data Example ===~n"),
    
    {ok, Env} = elmdb:env_open("/tmp/elmdb_hierarchical_example", []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Create hierarchical structure like a file system
    HierarchicalData = [
        {<<"users/alice/profile/name">>, <<"Alice Smith">>},
        {<<"users/alice/profile/email">>, <<"alice@example.com">>},
        {<<"users/alice/settings/theme">>, <<"dark">>},
        {<<"users/alice/settings/language">>, <<"en">>},
        {<<"users/bob/profile/name">>, <<"Bob Johnson">>},
        {<<"users/bob/profile/email">>, <<"bob@example.com">>},
        {<<"users/bob/settings/theme">>, <<"light">>},
        {<<"groups/admin/members">>, <<"alice,bob">>},
        {<<"groups/users/description">>, <<"Regular users group">>}
    ],
    
    % Store all data
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, HierarchicalData),
    
    % List all users
    {ok, UserKeys} = elmdb:list(DB, <<"users/">>),
    io:format("All user-related keys: ~p~n", [UserKeys]),
    
    % List Alice's data
    {ok, AliceKeys} = elmdb:list(DB, <<"users/alice/">>),
    io:format("Alice's keys: ~p~n", [AliceKeys]),
    
    % Get specific user data
    {ok, AliceName} = elmdb:get(DB, <<"users/alice/profile/name">>),
    {ok, AliceTheme} = elmdb:get(DB, <<"users/alice/settings/theme">>),
    io:format("Alice: ~s, Theme: ~s~n", [AliceName, AliceTheme]),
    
    % Clean up
    ok = elmdb:env_close(Env),
    file:del_dir_r("/tmp/elmdb_hierarchical_example"),
    ok.

%%%===================================================================
%%% User Session Store Example
%%%===================================================================

%% @doc Example of using elmdb as a session store
user_session_store() ->
    io:format("=== User Session Store Example ===~n"),
    
    {ok, Env} = elmdb:env_open("/tmp/elmdb_session_example", []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Create session data
    SessionId1 = <<"session_12345">>,
    SessionId2 = <<"session_67890">>,
    
    % Store session data as JSON-like binary
    SessionData1 = <<"{\"user_id\":\"alice\",\"login_time\":\"2024-01-01T10:00:00Z\",\"permissions\":[\"read\",\"write\"]}">>,
    SessionData2 = <<"{\"user_id\":\"bob\",\"login_time\":\"2024-01-01T11:00:00Z\",\"permissions\":[\"read\"]}">>,
    
    ok = elmdb:put(DB, SessionId1, SessionData1),
    ok = elmdb:put(DB, SessionId2, SessionData2),
    
    % Store session index by user
    ok = elmdb:put(DB, <<"user_sessions/alice">>, SessionId1),
    ok = elmdb:put(DB, <<"user_sessions/bob">>, SessionId2),
    
    % Retrieve session by session ID
    {ok, Session1} = elmdb:get(DB, SessionId1),
    io:format("Session ~s: ~s~n", [SessionId1, Session1]),
    
    % Find session by user
    {ok, AliceSessionId} = elmdb:get(DB, <<"user_sessions/alice">>),
    {ok, AliceSession} = elmdb:get(DB, AliceSessionId),
    io:format("Alice's session: ~s~n", [AliceSession]),
    
    % List all active sessions
    {ok, AllSessions} = elmdb:list(DB, <<"session_">>),
    io:format("Active sessions: ~p~n", [AllSessions]),
    
    % Logout (delete session)
    ok = elmdb:put(DB, SessionId1, <<"EXPIRED">>), % Mark as expired instead of delete
    
    % Clean up
    ok = elmdb:env_close(Env),
    file:del_dir_r("/tmp/elmdb_session_example"),
    ok.

%%%===================================================================
%%% Configuration Management Example
%%%===================================================================

%% @doc Example of using elmdb for application configuration
configuration_management() ->
    io:format("=== Configuration Management Example ===~n"),
    
    {ok, Env} = elmdb:env_open("/tmp/elmdb_config_example", []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Store configuration data
    Configurations = [
        {<<"config/database/host">>, <<"localhost">>},
        {<<"config/database/port">>, <<"5432">>},
        {<<"config/database/name">>, <<"myapp">>},
        {<<"config/server/listen_port">>, <<"8080">>},
        {<<"config/server/max_connections">>, <<"1000">>},
        {<<"config/logging/level">>, <<"info">>},
        {<<"config/logging/file">>, <<"/var/log/myapp.log">>},
        {<<"config/cache/ttl_seconds">>, <<"3600">>},
        {<<"config/cache/max_size">>, <<"10000">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, Configurations),
    
    % Get specific configuration
    {ok, DBHost} = elmdb:get(DB, <<"config/database/host">>),
    {ok, DBPort} = elmdb:get(DB, <<"config/database/port">>),
    io:format("Database: ~s:~s~n", [DBHost, DBPort]),
    
    % Get all database configurations
    {ok, DBConfigs} = elmdb:list(DB, <<"config/database/">>),
    io:format("Database configs: ~p~n", [DBConfigs]),
    
    % Update configuration
    ok = elmdb:put(DB, <<"config/server/listen_port">>, <<"8443">>),
    {ok, NewPort} = elmdb:get(DB, <<"config/server/listen_port">>),
    io:format("Updated server port: ~s~n", [NewPort]),
    
    % Get all configurations
    {ok, AllConfigs} = elmdb:list(DB, <<"config/">>),
    io:format("All configurations: ~p~n", [length(AllConfigs)]),
    
    % Clean up
    ok = elmdb:env_close(Env),
    file:del_dir_r("/tmp/elmdb_config_example"),
    ok.

%%%===================================================================
%%% Document Storage Example
%%%===================================================================

%% @doc Example of storing documents with metadata
document_storage() ->
    io:format("=== Document Storage Example ===~n"),
    
    {ok, Env} = elmdb:env_open("/tmp/elmdb_document_example", []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Store documents with metadata
    DocId1 = <<"doc_001">>,
    DocId2 = <<"doc_002">>,
    
    % Document content
    DocContent1 = <<"This is the content of document 1">>,
    DocContent2 = <<"This is the content of document 2">>,
    
    % Store documents
    ok = elmdb:put(DB, <<"documents/", DocId1/binary>>, DocContent1),
    ok = elmdb:put(DB, <<"documents/", DocId2/binary>>, DocContent2),
    
    % Store metadata
    ok = elmdb:put(DB, <<"metadata/", DocId1/binary, "/title">>, <<"First Document">>),
    ok = elmdb:put(DB, <<"metadata/", DocId1/binary, "/author">>, <<"Alice">>),
    ok = elmdb:put(DB, <<"metadata/", DocId1/binary, "/created">>, <<"2024-01-01">>),
    
    ok = elmdb:put(DB, <<"metadata/", DocId2/binary, "/title">>, <<"Second Document">>),
    ok = elmdb:put(DB, <<"metadata/", DocId2/binary, "/author">>, <<"Bob">>),
    ok = elmdb:put(DB, <<"metadata/", DocId2/binary, "/created">>, <<"2024-01-02">>),
    
    % Store index by author
    ok = elmdb:put(DB, <<"index/author/alice">>, DocId1),
    ok = elmdb:put(DB, <<"index/author/bob">>, DocId2),
    
    % Retrieve document by ID
    {ok, Doc1Content} = elmdb:get(DB, <<"documents/", DocId1/binary>>),
    {ok, Doc1Title} = elmdb:get(DB, <<"metadata/", DocId1/binary, "/title">>),
    io:format("Document ~s: ~s - ~s~n", [DocId1, Doc1Title, Doc1Content]),
    
    % Find documents by author
    {ok, AliceDocId} = elmdb:get(DB, <<"index/author/alice">>),
    {ok, AliceDocTitle} = elmdb:get(DB, <<"metadata/", AliceDocId/binary, "/title">>),
    io:format("Alice's document: ~s - ~s~n", [AliceDocId, AliceDocTitle]),
    
    % List all documents
    {ok, AllDocs} = elmdb:list(DB, <<"documents/">>),
    io:format("All documents: ~p~n", [AllDocs]),
    
    % List metadata for a document
    {ok, Doc1Metadata} = elmdb:list(DB, <<"metadata/", DocId1/binary, "/">>),
    io:format("Document ~s metadata: ~p~n", [DocId1, Doc1Metadata]),
    
    % Clean up
    ok = elmdb:env_close(Env),
    file:del_dir_r("/tmp/elmdb_document_example"),
    ok.

%%%===================================================================
%%% Key-Value Cache Example
%%%===================================================================

%% @doc Example of using elmdb as a cache with TTL simulation
key_value_cache() ->
    io:format("=== Key-Value Cache Example ===~n"),
    
    {ok, Env} = elmdb:env_open("/tmp/elmdb_cache_example", []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Cache with timestamp-based TTL
    Now = erlang:system_time(second),
    TTL = 3600, % 1 hour
    ExpiryTime = Now + TTL,
    
    % Store cached data with expiry metadata
    CacheKey = <<"user_profile_alice">>,
    CacheValue = <<"{\"name\":\"Alice\",\"email\":\"alice@example.com\"}">>,
    
    ok = elmdb:put(DB, <<"cache/", CacheKey/binary>>, CacheValue),
    ok = elmdb:put(DB, <<"expiry/", CacheKey/binary>>, integer_to_binary(ExpiryTime)),
    
    % Store more cache entries
    ok = elmdb:put(DB, <<"cache/user_profile_bob">>, <<"{\"name\":\"Bob\",\"email\":\"bob@example.com\"}">>),
    ok = elmdb:put(DB, <<"expiry/user_profile_bob">>, integer_to_binary(Now + TTL)),
    
    ok = elmdb:put(DB, <<"cache/api_response_123">>, <<"{\"status\":\"success\",\"data\":[1,2,3]}">>),
    ok = elmdb:put(DB, <<"expiry/api_response_123">>, integer_to_binary(Now + 300)), % 5 min TTL
    
    % Retrieve from cache with expiry check
    case get_cache_item(DB, <<"user_profile_alice">>) of
        {ok, Value} ->
            io:format("Cache hit for user_profile_alice: ~s~n", [Value]);
        expired ->
            io:format("Cache expired for user_profile_alice~n");
        not_found ->
            io:format("Cache miss for user_profile_alice~n")
    end,
    
    % List all cached items
    {ok, CachedItems} = elmdb:list(DB, <<"cache/">>),
    io:format("Cached items: ~p~n", [CachedItems]),
    
    % Simulate cache cleanup (remove expired items)
    cleanup_expired_cache(DB),
    
    % Clean up
    ok = elmdb:env_close(Env),
    file:del_dir_r("/tmp/elmdb_cache_example"),
    ok.

%% Helper function for cache retrieval with expiry check
get_cache_item(DB, Key) ->
    case elmdb:get(DB, <<"cache/", Key/binary>>) of
        {ok, Value} ->
            case elmdb:get(DB, <<"expiry/", Key/binary>>) of
                {ok, ExpiryBin} ->
                    Expiry = binary_to_integer(ExpiryBin),
                    Now = erlang:system_time(second),
                    case Now < Expiry of
                        true -> {ok, Value};
                        false -> expired
                    end;
                not_found ->
                    {ok, Value} % No expiry set, assume valid
            end;
        not_found ->
            not_found
    end.

%% Helper function to cleanup expired cache entries
cleanup_expired_cache(DB) ->
    {ok, ExpiryKeys} = elmdb:list(DB, <<"expiry/">>),
    Now = erlang:system_time(second),
    
    ExpiredKeys = lists:filtermap(fun(ExpiryKey) ->
        case elmdb:get(DB, ExpiryKey) of
            {ok, ExpiryBin} ->
                Expiry = binary_to_integer(ExpiryBin),
                case Now >= Expiry of
                    true ->
                        % Extract the original key
                        <<"expiry/", OriginalKey/binary>> = ExpiryKey,
                        {true, OriginalKey};
                    false ->
                        false
                end;
            not_found ->
                false
        end
    end, ExpiryKeys),
    
    % Mark expired items as deleted (or implement actual deletion if supported)
    lists:foreach(fun(Key) ->
        ok = elmdb:put(DB, <<"cache/", Key/binary>>, <<"EXPIRED">>),
        ok = elmdb:put(DB, <<"expiry/", Key/binary>>, <<"EXPIRED">>)
    end, ExpiredKeys),
    
    io:format("Cleaned up ~p expired cache entries~n", [length(ExpiredKeys)]).

%%%===================================================================
%%% File System Like Operations Example
%%%===================================================================

%% @doc Example of file system like operations
file_system_like_operations() ->
    io:format("=== File System Like Operations Example ===~n"),
    
    {ok, Env} = elmdb:env_open("/tmp/elmdb_filesystem_example", []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Create directory structure
    create_directory(DB, <<"home">>),
    create_directory(DB, <<"home/alice">>),
    create_directory(DB, <<"home/alice/documents">>),
    create_directory(DB, <<"home/bob">>),
    
    % Create files
    create_file(DB, <<"home/alice/readme.txt">>, <<"Welcome to Alice's home directory">>),
    create_file(DB, <<"home/alice/documents/notes.txt">>, <<"Alice's personal notes">>),
    create_file(DB, <<"home/bob/todo.txt">>, <<"Bob's todo list">>),
    
    % List directory contents
    io:format("Contents of /home:~n"),
    list_directory(DB, <<"home">>),
    
    io:format("Contents of /home/alice:~n"),
    list_directory(DB, <<"home/alice">>),
    
    % Read file content
    case read_file(DB, <<"home/alice/readme.txt">>) of
        {ok, Content} ->
            io:format("File content: ~s~n", [Content]);
        not_found ->
            io:format("File not found~n")
    end,
    
    % Check if path exists
    true = path_exists(DB, <<"home/alice">>),
    false = path_exists(DB, <<"home/charlie">>),
    
    % Clean up
    ok = elmdb:env_close(Env),
    file:del_dir_r("/tmp/elmdb_filesystem_example"),
    ok.

%% File system helper functions
create_directory(DB, Path) ->
    ok = elmdb:put(DB, <<"dir/", Path/binary>>, <<"directory">>),
    io:format("Created directory: ~s~n", [Path]).

create_file(DB, Path, Content) ->
    ok = elmdb:put(DB, <<"file/", Path/binary>>, Content),
    io:format("Created file: ~s~n", [Path]).

list_directory(DB, Path) ->
    DirPattern = <<"dir/", Path/binary, "/">>,
    FilePattern = <<"file/", Path/binary, "/">>,
    
    case elmdb:list(DB, DirPattern) of
        {ok, DirKeys} ->
            Dirs = [extract_name(K, DirPattern) || K <- DirKeys],
            lists:foreach(fun(Dir) ->
                io:format("  [DIR]  ~s~n", [Dir])
            end, Dirs);
        not_found ->
            ok
    end,
    
    case elmdb:list(DB, FilePattern) of
        {ok, FileKeys} ->
            Files = [extract_name(K, FilePattern) || K <- FileKeys],
            lists:foreach(fun(File) ->
                io:format("  [FILE] ~s~n", [File])
            end, Files);
        not_found ->
            ok
    end.

read_file(DB, Path) ->
    elmdb:get(DB, <<"file/", Path/binary>>).

path_exists(DB, Path) ->
    case elmdb:get(DB, <<"dir/", Path/binary>>) of
        {ok, _} -> true;
        not_found ->
            case elmdb:get(DB, <<"file/", Path/binary>>) of
                {ok, _} -> true;
                not_found -> false
            end
    end.

extract_name(Key, Pattern) ->
    PatternSize = byte_size(Pattern),
    <<_:PatternSize/binary, Name/binary>> = Key,
    case binary:split(Name, <<"/">>) of
        [FirstPart, _] -> FirstPart;
        [OnlyPart] -> OnlyPart
    end.

%%%===================================================================
%%% Batch Operations Example
%%%===================================================================

%% @doc Example of efficient batch operations
batch_operations() ->
    io:format("=== Batch Operations Example ===~n"),
    
    {ok, Env} = elmdb:env_open("/tmp/elmdb_batch_example", []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Batch insert of user data
    Users = [
        {<<"user001">>, <<"Alice">>, <<"alice@example.com">>},
        {<<"user002">>, <<"Bob">>, <<"bob@example.com">>},
        {<<"user003">>, <<"Charlie">>, <<"charlie@example.com">>},
        {<<"user004">>, <<"Diana">>, <<"diana@example.com">>},
        {<<"user005">>, <<"Eve">>, <<"eve@example.com">>}
    ],
    
    % Batch insert users
    io:format("Inserting ~p users...~n", [length(Users)]),
    lists:foreach(fun({UserId, Name, Email}) ->
        ok = elmdb:put(DB, <<"users/", UserId/binary, "/name">>, Name),
        ok = elmdb:put(DB, <<"users/", UserId/binary, "/email">>, Email),
        ok = elmdb:put(DB, <<"users/", UserId/binary, "/status">>, <<"active">>)
    end, Users),
    
    % Batch read - get all user names
    {ok, UserKeys} = elmdb:list(DB, <<"users/">>),
    NameKeys = [K || K <- UserKeys, binary:match(K, <<"/name">>) =/= nomatch],
    
    UserNames = lists:map(fun(NameKey) ->
        {ok, Name} = elmdb:get(DB, NameKey),
        Name
    end, NameKeys),
    
    io:format("User names: ~p~n", [UserNames]),
    
    % Batch update - deactivate all users
    io:format("Deactivating all users...~n"),
    StatusKeys = [K || K <- UserKeys, binary:match(K, <<"/status">>) =/= nomatch],
    lists:foreach(fun(StatusKey) ->
        ok = elmdb:put(DB, StatusKey, <<"inactive">>)
    end, StatusKeys),
    
    % Verify updates
    {ok, FirstUserStatus} = elmdb:get(DB, <<"users/user001/status">>),
    io:format("First user status after update: ~s~n", [FirstUserStatus]),
    
    % Clean up
    ok = elmdb:env_close(Env),
    file:del_dir_r("/tmp/elmdb_batch_example"),
    ok.

%%%===================================================================
%%% Error Handling Example
%%%===================================================================

%% @doc Example of proper error handling
error_handling_example() ->
    io:format("=== Error Handling Example ===~n"),
    
    % Safe database operations with error handling
    case safe_database_operation() of
        ok -> 
            io:format("Database operation completed successfully~n");
        {error, Reason} ->
            io:format("Database operation failed: ~p~n", [Reason])
    end.

safe_database_operation() ->
    try
        % Attempt to open database
        case elmdb:env_open("/tmp/elmdb_error_example", []) of
            {ok, Env} ->
                try
                    case elmdb:db_open(Env, [create]) of
                        {ok, DB} ->
                            try
                                % Perform database operations
                                ok = elmdb:put(DB, <<"test_key">>, <<"test_value">>),
                                
                                case elmdb:get(DB, <<"test_key">>) of
                                    {ok, Value} ->
                                        io:format("Retrieved value: ~s~n", [Value]),
                                        ok;
                                    not_found ->
                                        {error, unexpected_not_found}
                                end
                            catch
                                Class:Reason:Stack ->
                                    io:format("Error in database operations: ~p:~p~n", [Class, Reason]),
                                    {error, {database_operation_failed, Class, Reason, Stack}}
                            end;
                        {error, DBReason} ->
                            {error, {db_open_failed, DBReason}}
                    end
                after
                    % Always close environment
                    elmdb:env_close(Env)
                end;
            {error, EnvReason} ->
                {error, {env_open_failed, EnvReason}}
        end
    catch
        Class:Reason:Stack ->
            {error, {unexpected_error, Class, Reason, Stack}}
    after
        % Clean up
        file:del_dir_r("/tmp/elmdb_error_example")
    end.

%%%===================================================================
%%% Main Example Runner
%%%===================================================================

%% @doc Run all examples
run_all_examples() ->
    io:format("Running all elmdb examples...~n~n"),
    
    basic_usage(),
    io:format("~n"),
    
    hierarchical_data(),
    io:format("~n"),
    
    user_session_store(),
    io:format("~n"),
    
    configuration_management(),
    io:format("~n"),
    
    document_storage(),
    io:format("~n"),
    
    key_value_cache(),
    io:format("~n"),
    
    file_system_like_operations(),
    io:format("~n"),
    
    batch_operations(),
    io:format("~n"),
    
    error_handling_example(),
    
    io:format("~nAll examples completed!~n").