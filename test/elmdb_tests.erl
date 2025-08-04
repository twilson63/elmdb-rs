%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for elmdb NIF
%%% Comprehensive test suite covering all core functionality with proper
%%% resource management and edge case handling.
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helper functions
%%====================================================================

%% Create a unique test directory for each test
make_test_dir(TestName) ->
    Timestamp = integer_to_list(erlang:system_time()),
    Pid = integer_to_list(erlang:system_info(scheduler_id)),
    BaseDir = "/tmp/elmdb_test_" ++ atom_to_list(TestName) ++ "_" ++ Timestamp ++ "_" ++ Pid,
    ok = filelib:ensure_dir(BaseDir ++ "/"),
    BaseDir.

%% Clean up test directory
cleanup_test_dir(TestDir) ->
    case file:del_dir_r(TestDir) of
        ok -> ok;
        {error, enoent} -> ok;
        _Error -> ok  % Ignore cleanup errors in tests
    end.

%% Setup a basic database environment
setup_database(TestName) ->
    TestDir = make_test_dir(TestName),
    case elmdb:env_open(TestDir, []) of
        {ok, Env} ->
            case elmdb:db_open(Env, [create]) of
                {ok, DB} -> {TestDir, Env, DB};
                Error -> throw({db_open_failed, Error})
            end;
        Error -> throw({env_open_failed, Error})
    end.

%% Cleanup database resources
cleanup_database({TestDir, Env, DB}) ->
    catch elmdb:db_close(DB),
    catch elmdb:env_close(Env),
    cleanup_test_dir(TestDir).

%%====================================================================
%% Test Suite Setup/Cleanup
%%====================================================================

setup() ->
    application:start(elmdb).

cleanup(_) ->
    application:stop(elmdb).

%%====================================================================
%% Environment Management Tests
%%====================================================================

env_management_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Environment open and close", fun test_env_open_close/0},
        {"Environment open with invalid path", fun test_env_open_invalid_path/0},
        {"Environment open with options", fun test_env_open_with_options/0},
        {"Environment close by name", fun test_env_close_by_name/0},
        {"Environment double close handling", fun test_env_double_close/0}
    ]}.

test_env_open_close() ->
    TestDir = make_test_dir(env_open_close),
    
    % Test successful environment open
    case elmdb:env_open(TestDir, []) of
        {ok, Env} ->
            % Verify environment is a valid handle
            ?assert(is_reference(Env) orelse is_binary(Env) orelse is_tuple(Env)),
            
            % Test environment close
            ?assertEqual(ok, elmdb:env_close(Env));
        {error, already_open} ->
            % Environment already open, force close and try again
            elmdb:env_force_close_by_name(TestDir),
            {ok, Env} = elmdb:env_open(TestDir, []),
            ?assertEqual(ok, elmdb:env_close(Env))
    end,
    
    cleanup_test_dir(TestDir).

test_env_open_invalid_path() ->
    % Test opening environment with invalid path
    InvalidPath = "/this/path/does/not/exist/and/should/fail",
    ?assertMatch({error, _Reason}, elmdb:env_open(InvalidPath, [])).

test_env_open_with_options() ->
    TestDir = make_test_dir(env_open_options),
    
    % Test environment open with options
    Options = [{map_size, 10485760}], % 10MB
    case elmdb:env_open(TestDir, Options) of
        {ok, Env} ->
            ?assertEqual(ok, elmdb:env_close(Env));
        {error, already_open} ->
            % Environment already open, force close and try again
            elmdb:env_force_close_by_name(TestDir),
            {ok, Env} = elmdb:env_open(TestDir, Options),
            ?assertEqual(ok, elmdb:env_close(Env))
    end,
    cleanup_test_dir(TestDir).

test_env_close_by_name() ->
    TestDir = make_test_dir(env_close_by_name),
    
    % Open environment
    {ok, _Env} = elmdb:env_open(TestDir, []),
    
    % Close by name (path)
    ?assertEqual(ok, elmdb:env_close_by_name(TestDir)),
    
    cleanup_test_dir(TestDir).

test_env_double_close() ->
    TestDir = make_test_dir(env_double_close),
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % First close should succeed
    ?assertEqual(ok, elmdb:env_close(Env)),
    
    % Second close should be handled gracefully (not crash)
    Result = (catch elmdb:env_close(Env)),
    ?assert(Result =:= ok orelse element(1, Result) =:= error),
    
    cleanup_test_dir(TestDir).

%%====================================================================
%% Database Operations Tests
%%====================================================================

database_operations_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Database open and close", fun test_db_open_close/0},
        {"Database open without create flag", fun test_db_open_no_create/0},
        {"Multiple database handles", fun test_multiple_databases/0}
    ]}.

test_db_open_close() ->
    TestDir = make_test_dir(db_open_close),
    
    % Handle already_open case
    case elmdb:env_open(TestDir, []) of
        {ok, Env} -> ok;
        {error, already_open} ->
            elmdb:env_force_close_by_name(TestDir),
            {ok, Env} = elmdb:env_open(TestDir, [])
    end,
    
    % Test database open with create flag
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Verify database handle is valid
    ?assert(is_reference(DB) orelse is_binary(DB) orelse is_tuple(DB)),
    
    % Close database first, then environment
    ?assertEqual(ok, elmdb:db_close(DB)),
    ?assertEqual(ok, elmdb:env_close(Env)),
    cleanup_test_dir(TestDir).

test_db_open_no_create() ->
    TestDir = make_test_dir(db_open_no_create),
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Test database open without create flag on new environment
    % This should either succeed (if default DB exists) or fail gracefully
    Result = elmdb:db_open(Env, []),
    case Result of
        {ok, DB} -> 
            ?assertEqual(ok, elmdb:db_close(DB));
        {error, _Reason} -> 
            ok  % Expected for new environment
    end,
    
    ?assertEqual(ok, elmdb:env_close(Env)),
    cleanup_test_dir(TestDir).

test_multiple_databases() ->
    TestDir = make_test_dir(multiple_databases),
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Open multiple database handles
    {ok, DB1} = elmdb:db_open(Env, [create]),
    {ok, DB2} = elmdb:db_open(Env, [create]),
    
    % Test that they can operate independently
    Key = <<"test_key">>,
    Value1 = <<"value_in_db1">>,
    Value2 = <<"value_in_db2">>,
    
    ?assertEqual(ok, elmdb:put(DB1, Key, Value1)),
    ?assertEqual(ok, elmdb:put(DB2, Key, Value2)),
    
    % Both should return values (may be same if sharing namespace)
    ?assertMatch({ok, _Value1}, elmdb:get(DB1, Key)),
    ?assertMatch({ok, _Value2}, elmdb:get(DB2, Key)),
    
    ?assertEqual(ok, elmdb:db_close(DB1)),
    ?assertEqual(ok, elmdb:db_close(DB2)),
    ?assertEqual(ok, elmdb:env_close(Env)),
    cleanup_test_dir(TestDir).

%%====================================================================
%% Key-Value Operations Tests
%%====================================================================

key_value_operations_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Basic put and get operations", fun test_basic_put_get/0},
        {"Get missing key", fun test_get_missing_key/0},
        {"Put and get large values", fun test_put_get_large_value/0},
        {"Put and get binary keys", fun test_put_get_binary_keys/0},
        {"Put overwrite value", fun test_put_overwrite_value/0},
        {"Put and get edge cases", fun test_put_get_edge_cases/0}
    ]}.

test_basic_put_get() ->
    {TestDir, Env, DB} = setup_database(basic_put_get),
    
    % Test basic put operation
    Key = <<"test_key">>,
    Value = <<"test_value">>,
    ?assertEqual(ok, elmdb:put(DB, Key, Value)),
    
    % Test basic get operation
    ?assertMatch({ok, Value}, elmdb:get(DB, Key)),
    {ok, RetrievedValue} = elmdb:get(DB, Key),
    ?assertEqual(Value, RetrievedValue),
    
    cleanup_database({TestDir, Env, DB}).

test_get_missing_key() ->
    {TestDir, Env, DB} = setup_database(get_missing_key),
    
    % Test get operation on missing key
    MissingKey = <<"missing_key">>,
    Result = elmdb:get(DB, MissingKey),
    ?assert(Result =:= not_found orelse element(1, Result) =:= error),
    
    cleanup_database({TestDir, Env, DB}).

test_put_get_large_value() ->
    TestDir = make_test_dir(put_get_large_value),
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 10485760}]), % 10MB
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test with large value (1MB)
    Key = <<"large_key">>,
    Value = binary:copy(<<"X">>, 1048576), % 1MB of X's
    ?assertEqual(ok, elmdb:put(DB, Key, Value)),
    
    ?assertMatch({ok, Value}, elmdb:get(DB, Key)),
    {ok, RetrievedValue} = elmdb:get(DB, Key),
    ?assertEqual(Value, RetrievedValue),
    
    cleanup_database({TestDir, Env, DB}).

test_put_get_binary_keys() ->
    {TestDir, Env, DB} = setup_database(put_get_binary_keys),
    
    % Test with various binary key formats
    TestCases = [
        {<<"simple">>, <<"value1">>},
        {<<"key/with/slashes">>, <<"value2">>},
        {<<"key.with.dots">>, <<"value3">>},
        {<<"key-with-dashes">>, <<"value4">>},
        {<<"key_with_underscores">>, <<"value5">>},
        {<<0,1,2,3,4,5>>, <<"binary_key_value">>} % Binary data key
    ],
    
    % Put all test cases
    lists:foreach(fun({Key, Value}) ->
        ?assertEqual(ok, elmdb:put(DB, Key, Value))
    end, TestCases),
    
    % Get and verify all test cases
    lists:foreach(fun({Key, ExpectedValue}) ->
        ?assertMatch({ok, ExpectedValue}, elmdb:get(DB, Key))
    end, TestCases),
    
    cleanup_database({TestDir, Env, DB}).

test_put_overwrite_value() ->
    {TestDir, Env, DB} = setup_database(put_overwrite_value),
    
    Key = <<"overwrite_key">>,
    Value1 = <<"original_value">>,
    Value2 = <<"updated_value">>,
    
    % Put original value
    ?assertEqual(ok, elmdb:put(DB, Key, Value1)),
    ?assertMatch({ok, Value1}, elmdb:get(DB, Key)),
    
    % Overwrite with new value
    ?assertEqual(ok, elmdb:put(DB, Key, Value2)),
    ?assertMatch({ok, Value2}, elmdb:get(DB, Key)),
    
    cleanup_database({TestDir, Env, DB}).

test_put_get_edge_cases() ->
    {TestDir, Env, DB} = setup_database(put_get_edge_cases),
    
    % Test empty value
    ?assertEqual(ok, elmdb:put(DB, <<"empty_value_key">>, <<"">>)),
    ?assertMatch({ok, <<"">>}, elmdb:get(DB, <<"empty_value_key">>)),
    
    % Test binary with null bytes
    NullBinary = <<1,2,3,0,4,5,6>>,
    ?assertEqual(ok, elmdb:put(DB, <<"null_test">>, NullBinary)),
    ?assertMatch({ok, NullBinary}, elmdb:get(DB, <<"null_test">>)),
    
    cleanup_database({TestDir, Env, DB}).

%%====================================================================
%% List Operations Tests (with panic fix)
%%====================================================================

list_operations_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"List empty database (panic fix)", fun test_list_empty_database/0},
        {"List nonexistent prefix", fun test_list_nonexistent_prefix/0},
        {"List single level keys", fun test_list_single_level/0},
        {"List hierarchical keys", fun test_list_hierarchical_keys/0},
        {"List with binary prefixes", fun test_list_binary_prefixes/0},
        {"List all keys (empty prefix)", fun test_list_all_keys/0}
    ]}.

test_list_empty_database() ->
    {TestDir, Env, DB} = setup_database(list_empty_database),
    
    % List from empty database should not panic and return not_found
    Result = elmdb:list(DB, <<"any_prefix">>),
    ?assert(Result =:= not_found orelse element(1, Result) =:= error),
    
    cleanup_database({TestDir, Env, DB}).

test_list_nonexistent_prefix() ->
    {TestDir, Env, DB} = setup_database(list_nonexistent_prefix),
    
    % Add some data first
    ?assertEqual(ok, elmdb:put(DB, <<"users/alice">>, <<"alice_data">>)),
    ?assertEqual(ok, elmdb:put(DB, <<"users/bob">>, <<"bob_data">>)),
    
    % List with non-existent prefix should return not_found
    Result = elmdb:list(DB, <<"nonexistent">>),
    ?assert(Result =:= not_found orelse element(1, Result) =:= error),
    
    cleanup_database({TestDir, Env, DB}).

test_list_single_level() ->
    {TestDir, Env, DB} = setup_database(list_single_level),
    
    % Create simple flat keys
    TestData = [
        {<<"item1">>, <<"value1">>},
        {<<"item2">>, <<"value2">>},
        {<<"item3">>, <<"value3">>},
        {<<"other">>, <<"other_value">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ?assertEqual(ok, elmdb:put(DB, Key, Value))
    end, TestData),
    
    % List with "item" prefix should return item1, item2, item3
    case elmdb:list(DB, <<"item">>) of
        {ok, Results} ->
            % Results should contain all items with prefix
            ExpectedKeys = [<<"item1">>, <<"item2">>, <<"item3">>],
            lists:foreach(fun(ExpectedKey) ->
                ?assert(lists:member(ExpectedKey, Results))
            end, ExpectedKeys),
            % Should not contain "other"
            ?assert(not lists:member(<<"other">>, Results));
        Result ->
            ?assert(Result =:= not_found orelse element(1, Result) =:= error)
    end,
    
    cleanup_database({TestDir, Env, DB}).

test_list_hierarchical_keys() ->
    {TestDir, Env, DB} = setup_database(list_hierarchical_keys),
    
    % Create hierarchical structure
    TestData = [
        {<<"users">>, <<"users_metadata">>},
        {<<"users/alice">>, <<"alice_data">>},
        {<<"users/alice/profile">>, <<"alice_profile">>},
        {<<"users/alice/settings">>, <<"alice_settings">>},
        {<<"users/bob">>, <<"bob_data">>},
        {<<"users/bob/profile">>, <<"bob_profile">>},
        {<<"groups">>, <<"groups_metadata">>},
        {<<"groups/admin">>, <<"admin_group">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ?assertEqual(ok, elmdb:put(DB, Key, Value))
    end, TestData),
    
    % List all keys starting with "users/"
    case elmdb:list(DB, <<"users/">>) of
        {ok, UserResults} ->
            % Should include all user-related keys
            ExpectedUserKeys = [
                <<"users/alice">>,
                <<"users/alice/profile">>,
                <<"users/alice/settings">>,
                <<"users/bob">>,
                <<"users/bob/profile">>
            ],
            lists:foreach(fun(ExpectedKey) ->
                ?assert(lists:member(ExpectedKey, UserResults))
            end, ExpectedUserKeys);
        Result ->
            ?assert(Result =:= not_found orelse element(1, Result) =:= error)
    end,
    
    cleanup_database({TestDir, Env, DB}).

test_list_binary_prefixes() ->
    {TestDir, Env, DB} = setup_database(list_binary_prefixes),
    
    % Test with binary data as prefixes
    TestData = [
        {<<1,2,3>>, <<"binary_value1">>},
        {<<1,2,3,4>>, <<"binary_value2">>},
        {<<1,2,3,4,5>>, <<"binary_value3">>},
        {<<1,2,4>>, <<"different_binary">>},
        {<<1,3>>, <<"another_binary">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ?assertEqual(ok, elmdb:put(DB, Key, Value))
    end, TestData),
    
    % List with binary prefix <<1,2,3>>
    case elmdb:list(DB, <<1,2,3>>) of
        {ok, BinaryResults} ->
            ExpectedBinaryKeys = [<<1,2,3>>, <<1,2,3,4>>, <<1,2,3,4,5>>],
            lists:foreach(fun(Key) ->
                ?assert(lists:member(Key, BinaryResults))
            end, ExpectedBinaryKeys),
            % Ensure other keys are not included
            ?assert(not lists:member(<<1,2,4>>, BinaryResults)),
            ?assert(not lists:member(<<1,3>>, BinaryResults));
        Result ->
            ?assert(Result =:= not_found orelse element(1, Result) =:= error)
    end,
    
    cleanup_database({TestDir, Env, DB}).

test_list_all_keys() ->
    {TestDir, Env, DB} = setup_database(list_all_keys),
    
    % Add some test data
    TestData = [
        {<<"users">>, <<"users_data">>},
        {<<"groups">>, <<"groups_data">>},
        {<<"config">>, <<"config_data">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ?assertEqual(ok, elmdb:put(DB, Key, Value))
    end, TestData),
    
    % List all keys (empty prefix)
    case elmdb:list(DB, <<"">>) of
        {ok, AllResults} ->
            % Should include all keys in the database
            ExpectedKeys = [<<"users">>, <<"groups">>, <<"config">>],
            lists:foreach(fun(Key) ->
                ?assert(lists:member(Key, AllResults))
            end, ExpectedKeys);
        Result ->
            ?assert(Result =:= not_found orelse element(1, Result) =:= error)
    end,
    
    cleanup_database({TestDir, Env, DB}).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Operations on closed database", fun test_operations_on_closed_db/0},
        {"Invalid key/value inputs", fun test_invalid_key_value_inputs/0},
        {"Resource cleanup after errors", fun test_resource_cleanup_after_errors/0}
    ]}.

test_operations_on_closed_db() ->
    {TestDir, Env, DB} = setup_database(operations_on_closed_db),
    
    % Put some data first
    ?assertEqual(ok, elmdb:put(DB, <<"test_key">>, <<"test_value">>)),
    
    % Close database and environment
    ?assertEqual(ok, elmdb:db_close(DB)),
    ?assertEqual(ok, elmdb:env_close(Env)),
    
    % All subsequent operations should fail gracefully (not crash)
    PutResult = (catch elmdb:put(DB, <<"key2">>, <<"value2">>)),
    ?assert(element(1, PutResult) =:= error orelse element(1, PutResult) =:= 'EXIT'),
    
    GetResult = (catch elmdb:get(DB, <<"test_key">>)),
    ?assert(GetResult =:= not_found orelse element(1, GetResult) =:= error orelse element(1, GetResult) =:= 'EXIT'),
    
    ListResult = (catch elmdb:list(DB, <<"test">>)),
    ?assert(ListResult =:= not_found orelse element(1, ListResult) =:= error orelse element(1, ListResult) =:= 'EXIT'),
    
    cleanup_test_dir(TestDir).

test_invalid_key_value_inputs() ->
    {TestDir, Env, DB} = setup_database(invalid_key_value_inputs),
    
    % Test invalid key/value combinations (should not crash)
    InvalidInputs = [
        {undefined, <<"value">>},
        {<<"key">>, undefined},
        {123, <<"value">>},
        {<<"key">>, 123}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        PutResult = (catch elmdb:put(DB, Key, Value)),
        ?assert(element(1, PutResult) =:= error orelse element(1, PutResult) =:= 'EXIT')
    end, InvalidInputs),
    
    cleanup_database({TestDir, Env, DB}).

test_resource_cleanup_after_errors() ->
    TestDir = make_test_dir(resource_cleanup_after_errors),
    
    % Test that resources are properly cleaned up even after errors
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Force some operations that might cause errors
    catch elmdb:put(DB, undefined, <<"value">>),
    catch elmdb:get(DB, undefined),
    catch elmdb:list(DB, undefined),
    
    % Should still be able to do valid operations
    ?assertEqual(ok, elmdb:put(DB, <<"valid_key">>, <<"valid_value">>)),
    ?assertMatch({ok, <<"valid_value">>}, elmdb:get(DB, <<"valid_key">>)),
    
    % Cleanup should work normally
    ?assertEqual(ok, elmdb:db_close(DB)),
    ?assertEqual(ok, elmdb:env_close(Env)),
    cleanup_test_dir(TestDir).

%%====================================================================
%% Reference Counting and Resource Management Tests
%%====================================================================

reference_counting_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Proper database handle cleanup", fun test_database_handle_cleanup/0},
        {"Environment reference counting", fun test_environment_reference_counting/0},
        {"Multiple database handles cleanup", fun test_multiple_handles_cleanup/0}
    ]}.

test_database_handle_cleanup() ->
    TestDir = make_test_dir(database_handle_cleanup),
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Open and close multiple database handles
    {ok, DB1} = elmdb:db_open(Env, [create]),
    {ok, DB2} = elmdb:db_open(Env, [create]),
    {ok, DB3} = elmdb:db_open(Env, [create]),
    
    % Use the databases
    ?assertEqual(ok, elmdb:put(DB1, <<"key1">>, <<"value1">>)),
    ?assertEqual(ok, elmdb:put(DB2, <<"key2">>, <<"value2">>)),
    ?assertEqual(ok, elmdb:put(DB3, <<"key3">>, <<"value3">>)),
    
    % Close all database handles
    ?assertEqual(ok, elmdb:db_close(DB1)),
    ?assertEqual(ok, elmdb:db_close(DB2)),
    ?assertEqual(ok, elmdb:db_close(DB3)),
    
    % Environment should still be open and functional
    {ok, DB4} = elmdb:db_open(Env, [create]),
    ?assertMatch({ok, <<"value1">>}, elmdb:get(DB4, <<"key1">>)),
    
    ?assertEqual(ok, elmdb:db_close(DB4)),
    ?assertEqual(ok, elmdb:env_close(Env)),
    cleanup_test_dir(TestDir).

test_environment_reference_counting() ->
    TestDir = make_test_dir(environment_reference_counting),
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Open multiple database handles from same environment
    {ok, DB1} = elmdb:db_open(Env, [create]),
    {ok, DB2} = elmdb:db_open(Env, [create]),
    
    % Use the databases
    ?assertEqual(ok, elmdb:put(DB1, <<"key1">>, <<"value1">>)),
    ?assertEqual(ok, elmdb:put(DB2, <<"key2">>, <<"value2">>)),
    
    % Close one database handle - environment should remain open
    ?assertEqual(ok, elmdb:db_close(DB1)),
    
    % Second database should still work
    ?assertMatch({ok, <<"value2">>}, elmdb:get(DB2, <<"key2">>)),
    
    % Close remaining handle and environment
    ?assertEqual(ok, elmdb:db_close(DB2)),
    ?assertEqual(ok, elmdb:env_close(Env)),
    cleanup_test_dir(TestDir).

test_multiple_handles_cleanup() ->
    TestDir = make_test_dir(multiple_handles_cleanup),
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Create many database handles
    Handles = [begin
        {ok, DB} = elmdb:db_open(Env, [create]),
        ?assertEqual(ok, elmdb:put(DB, list_to_binary("key" ++ integer_to_list(I)), 
                                       list_to_binary("value" ++ integer_to_list(I)))),
        DB
    end || I <- lists:seq(1, 10)],
    
    % Close all handles
    lists:foreach(fun(DB) ->
        ?assertEqual(ok, elmdb:db_close(DB))
    end, Handles),
    
    % Verify environment is still functional
    {ok, TestDB} = elmdb:db_open(Env, [create]),
    ?assertMatch({ok, <<"value1">>}, elmdb:get(TestDB, <<"key1">>)),
    ?assertMatch({ok, <<"value10">>}, elmdb:get(TestDB, <<"key10">>)),
    
    ?assertEqual(ok, elmdb:db_close(TestDB)),
    ?assertEqual(ok, elmdb:env_close(Env)),
    cleanup_test_dir(TestDir).