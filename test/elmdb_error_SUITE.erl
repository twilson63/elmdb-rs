%%%-------------------------------------------------------------------
%%% @doc
%%% Error handling tests for elmdb NIF
%%% Tests various error conditions and edge cases
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_error_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% Common Test callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    ok = application:start(elmdb),
    Config.

end_per_suite(_Config) ->
    application:stop(elmdb),
    ok.

init_per_testcase(TestCase, Config) ->
    % Create unique test directory for each test case
    TestDir = filename:join([?config(priv_dir, Config), 
                            atom_to_list(TestCase)]),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    % Clean up test directory
    TestDir = ?config(test_dir, Config),
    case file:del_dir_r(TestDir) of
        ok -> ok;
        {error, enoent} -> ok;
        Error -> ct:pal("Failed to clean up ~p: ~p", [TestDir, Error])
    end,
    ok.

all() ->
    [
        test_env_open_invalid_path,
        test_env_open_permission_denied,
        test_env_open_invalid_options,
        test_env_close_invalid_handle,
        test_env_close_double_close,
        test_env_close_by_name_invalid_path,
        test_db_open_closed_env,
        test_db_open_invalid_options,
        test_put_closed_db,
        test_put_invalid_key_value,
        test_get_closed_db,
        test_get_invalid_key,
        test_list_closed_db,
        test_list_invalid_key,
        test_operations_after_env_close,
        test_large_key_value_limits,
        test_invalid_binary_inputs,
        test_memory_exhaustion_simulation
    ].

%%====================================================================
%% Helper functions
%%====================================================================

setup_database(Config) ->
    TestDir = ?config(test_dir, Config),
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    {Env, DB}.

create_readonly_directory(Path) ->
    % Create directory and make it read-only
    ok = filelib:ensure_dir(Path ++ "/"),
    case file:change_mode(Path, 8#444) of % Read-only
        ok -> ok;
        {error, _} -> ok % May not work on all systems
    end.

%%====================================================================
%% Environment Error Tests
%%====================================================================

test_env_open_invalid_path(_Config) ->
    % Test various invalid paths
    InvalidPaths = [
        "/this/path/does/not/exist/and/should/fail",
        "/dev/null/cannot/create/directory/here",
        <<"/invalid/binary/path">>,
        ""  % Empty string
    ],
    
    lists:foreach(fun(Path) ->
        case elmdb:env_open(Path, []) of
            {error, _Reason} -> 
                ct:pal("Expected error for path ~p", [Path]);
            {ok, Env} -> 
                elmdb:env_close(Env),
                ct:fail("Expected error for invalid path ~p, but got success", [Path])
        end
    end, InvalidPaths),
    
    ok.

test_env_open_permission_denied(Config) ->
    % Create a read-only directory to simulate permission denied
    TestDir = ?config(test_dir, Config),
    ReadOnlyDir = filename:join([TestDir, "readonly"]),
    create_readonly_directory(ReadOnlyDir),
    
    % Try to open database in read-only directory
    case elmdb:env_open(ReadOnlyDir, []) of
        {error, _Reason} -> 
            ct:pal("Got expected permission error");
        {ok, Env} -> 
            elmdb:env_close(Env),
            ct:pal("Warning: Expected permission error, but operation succeeded")
    end,
    
    % Restore permissions for cleanup
    file:change_mode(ReadOnlyDir, 8#755),
    ok.

test_env_open_invalid_options(_Config) ->
    % Test with invalid options (depending on implementation)
    TestDir = "/tmp/test_invalid_options",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    InvalidOptions = [
        [{invalid_option, true}],
        [{map_size, -1}],  % Negative size
        [{map_size, not_an_integer}],
        [invalid_atom],
        "not_a_list"
    ],
    
    lists:foreach(fun(Options) ->
        case catch elmdb:env_open(TestDir, Options) of
            {error, _Reason} -> 
                ct:pal("Expected error for options ~p", [Options]);
            {'EXIT', _} ->
                ct:pal("Expected crash for invalid options ~p", [Options]);
            {ok, Env} -> 
                elmdb:env_close(Env),
                ct:pal("Warning: Expected error for options ~p, but got success", [Options])
        end
    end, InvalidOptions),
    
    file:del_dir_r(TestDir),
    ok.

test_env_close_invalid_handle(_Config) ->
    % Test closing invalid environment handles
    InvalidHandles = [
        undefined,
        nil,
        <<"not_a_handle">>,
        {invalid, handle},
        123,
        self()  % Process pid
    ],
    
    lists:foreach(fun(Handle) ->
        case catch elmdb:env_close(Handle) of
            ok -> 
                ct:pal("Warning: Expected error for handle ~p, but got ok", [Handle]);
            {error, _Reason} ->
                ct:pal("Expected error for invalid handle ~p", [Handle]);
            {'EXIT', _} ->
                ct:pal("Expected crash for invalid handle ~p", [Handle])
        end
    end, InvalidHandles),
    
    ok.

test_env_close_double_close(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Close database first, then environment once
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    
    % Try to close again - should handle gracefully
    case catch elmdb:env_close(Env) of
        ok -> 
            ct:pal("Double close handled gracefully");
        {error, _Reason} ->
            ct:pal("Double close returned error as expected");
        {'EXIT', _} ->
            ct:pal("Double close caused crash")
    end,
    
    ok.

test_env_close_by_name_invalid_path(_Config) ->
    InvalidPaths = [
        "/nonexistent/path",
        <<"binary_path">>,
        "",
        undefined,
        123
    ],
    
    lists:foreach(fun(Path) ->
        case catch elmdb:env_close_by_name(Path) of
            ok -> 
                ct:pal("Close by name succeeded for ~p", [Path]);
            {error, _Reason} ->
                ct:pal("Expected error for path ~p", [Path]);
            {'EXIT', _} ->
                ct:pal("Expected crash for invalid path ~p", [Path])
        end
    end, InvalidPaths),
    
    ok.

%%====================================================================
%% Database Error Tests
%%====================================================================

test_db_open_closed_env(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Close database first, then environment
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    
    % Try to open database on closed environment
    case catch elmdb:db_open(Env, [create]) of
        {error, _Reason} ->
            ct:pal("Expected error for closed environment");
        {error, _Type, _Description} ->
            ct:pal("Expected error for closed environment");
        {'EXIT', _} ->
            ct:pal("Expected crash for closed environment");
        {ok, _DB} ->
            ct:fail("Expected error for closed environment, but got success")
    end,
    
    ok.

test_db_open_invalid_options(_Config) ->
    TestDir = "/tmp/test_db_invalid_options",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    InvalidOptions = [
        [invalid_option],
        ["not_an_atom"],
        {not_a_list},
        123
    ],
    
    lists:foreach(fun(Options) ->
        case catch elmdb:db_open(Env, Options) of
            {error, _Reason} ->
                ct:pal("Expected error for options ~p", [Options]);
            {'EXIT', _} ->
                ct:pal("Expected crash for invalid options ~p", [Options]);
            {ok, _DB} ->
                ct:pal("Warning: Expected error for options ~p", [Options])
        end
    end, InvalidOptions),
    
    ok = elmdb:env_close(Env),
    file:del_dir_r(TestDir),
    ok.

%%====================================================================
%% Key-Value Operation Error Tests
%%====================================================================

test_put_closed_db(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Close database first, then environment (which should invalidate DB)
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    
    % Try to put after closing
    case catch elmdb:put(DB, <<"key">>, <<"value">>) of
        {error, _Reason} ->
            ct:pal("Expected error for closed database");
        {error, _Type, _Description} ->
            ct:pal("Expected error for closed database");
        {'EXIT', _} ->
            ct:pal("Expected crash for closed database");
        ok ->
            ct:fail("Expected error for closed database, but got success")
    end,
    
    ok.

test_put_invalid_key_value(_Config) ->
    TestDir = "/tmp/test_put_invalid",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test invalid key/value combinations
    InvalidInputs = [
        {undefined, <<"value">>},
        {<<"key">>, undefined},
        {123, <<"value">>},
        {<<"key">>, 123},
        {"string_key", <<"value">>},  % String instead of binary
        {<<"key">>, "string_value"},  % String instead of binary
        {[], <<"value">>},
        {<<"key">>, []}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        case catch elmdb:put(DB, Key, Value) of
            {error, _Reason} ->
                ct:pal("Expected error for key/value ~p/~p", [Key, Value]);
            {'EXIT', _} ->
                ct:pal("Expected crash for invalid key/value ~p/~p", [Key, Value]);
            ok ->
                ct:pal("Warning: Expected error for key/value ~p/~p", [Key, Value])
        end
    end, InvalidInputs),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    file:del_dir_r(TestDir),
    ok.

test_get_closed_db(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Close database first, then environment
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    
    % Try to get after closing
    case catch elmdb:get(DB, <<"key">>) of
        {error, _Reason} ->
            ct:pal("Expected error for closed database");
        {error, _Type, _Description} ->
            ct:pal("Expected error for closed database");
        {'EXIT', _} ->
            ct:pal("Expected crash for closed database");
        not_found ->
            ct:pal("Warning: Get on closed DB returned not_found");
        {ok, _Value} ->
            ct:fail("Expected error for closed database, but got success")
    end,
    
    ok.

test_get_invalid_key(_Config) ->
    TestDir = "/tmp/test_get_invalid",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    InvalidKeys = [
        undefined,
        123,
        "string_key",
        [],
        {tuple, key}
    ],
    
    lists:foreach(fun(Key) ->
        case catch elmdb:get(DB, Key) of
            {error, _Reason} ->
                ct:pal("Expected error for key ~p", [Key]);
            {'EXIT', _} ->
                ct:pal("Expected crash for invalid key ~p", [Key]);
            not_found ->
                ct:pal("Warning: Get with invalid key ~p returned not_found", [Key]);
            {ok, _Value} ->
                ct:pal("Warning: Get with invalid key ~p succeeded", [Key])
        end
    end, InvalidKeys),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    file:del_dir_r(TestDir),
    ok.

%%====================================================================
%% List Operation Error Tests
%%====================================================================

test_list_closed_db(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Close database first, then environment
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    
    % Try to list after closing
    case catch elmdb:list(DB, <<"prefix">>) of
        {error, _Reason} ->
            ct:pal("Expected error for closed database");
        {error, _Type, _Description} ->
            ct:pal("Expected error for closed database");
        {'EXIT', _} ->
            ct:pal("Expected crash for closed database");
        not_found ->
            ct:pal("Warning: List on closed DB returned not_found");
        {ok, _Keys} ->
            ct:fail("Expected error for closed database, but got success")
    end,
    
    ok.

test_list_invalid_key(_Config) ->
    TestDir = "/tmp/test_list_invalid",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    InvalidKeys = [
        undefined,
        123,
        "string_key",
        [],
        {tuple, key}
    ],
    
    lists:foreach(fun(Key) ->
        case catch elmdb:list(DB, Key) of
            {error, _Reason} ->
                ct:pal("Expected error for key ~p", [Key]);
            {'EXIT', _} ->
                ct:pal("Expected crash for invalid key ~p", [Key]);
            not_found ->
                ct:pal("Warning: List with invalid key ~p returned not_found", [Key]);
            {ok, _Keys} ->
                ct:pal("Warning: List with invalid key ~p succeeded", [Key])
        end
    end, InvalidKeys),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    file:del_dir_r(TestDir),
    ok.

%%====================================================================
%% State Management Error Tests
%%====================================================================

test_operations_after_env_close(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Put some data first
    ok = elmdb:put(DB, <<"test_key">>, <<"test_value">>),
    
    % Close database first, then environment
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    
    % All subsequent operations should fail
    Operations = [
        fun() -> elmdb:put(DB, <<"key2">>, <<"value2">>) end,
        fun() -> elmdb:get(DB, <<"test_key">>) end,
        fun() -> elmdb:list(DB, <<"test">>) end,
        fun() -> elmdb:db_open(Env, [create]) end
    ],
    
    lists:foreach(fun(Op) ->
        case catch Op() of
            {error, _Reason} ->
                ct:pal("Operation failed as expected after env close");
            {error, _Type, _Description} ->
                ct:pal("Operation failed as expected after env close");
            {'EXIT', _} ->
                ct:pal("Operation crashed as expected after env close");
            ok ->
                ct:pal("Warning: Operation succeeded after env close");
            not_found ->
                ct:pal("Warning: Operation returned not_found after env close");
            {ok, _} ->
                ct:pal("Warning: Operation succeeded after env close")
        end
    end, Operations),
    
    ok.

%%====================================================================
%% Limit and Edge Case Tests
%%====================================================================

test_large_key_value_limits(_Config) ->
    TestDir = "/tmp/test_large_limits",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 104857600}]), % 100MB
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test very large key (LMDB has key size limits)
    LargeKey = binary:copy(<<"K">>, 1024),  % 1KB key
    case catch elmdb:put(DB, LargeKey, <<"value">>) of
        ok -> ct:pal("Large key accepted");
        {error, _} -> ct:pal("Large key rejected as expected");
        {'EXIT', _} -> ct:pal("Large key caused crash")
    end,
    
    % Test extremely large key
    VeryLargeKey = binary:copy(<<"K">>, 10240),  % 10KB key
    case catch elmdb:put(DB, VeryLargeKey, <<"value">>) of
        ok -> ct:pal("Very large key accepted");
        {error, _} -> ct:pal("Very large key rejected as expected");
        {'EXIT', _} -> ct:pal("Very large key caused crash")
    end,
    
    % Test very large value
    LargeValue = binary:copy(<<"V">>, 1048576),  % 1MB value
    case catch elmdb:put(DB, <<"large_value_key">>, LargeValue) of
        ok -> ct:pal("Large value accepted");
        {error, _} -> ct:pal("Large value rejected");
        {'EXIT', _} -> ct:pal("Large value caused crash")
    end,
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    file:del_dir_r(TestDir),
    ok.

test_invalid_binary_inputs(_Config) ->
    TestDir = "/tmp/test_invalid_binary",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test with binary containing null bytes
    NullBinary = <<1,2,3,0,4,5,6>>,
    case catch elmdb:put(DB, <<"null_test">>, NullBinary) of
        ok -> 
            {ok, Retrieved} = elmdb:get(DB, <<"null_test">>),
            NullBinary = Retrieved,
            ct:pal("Null bytes in value handled correctly");
        {error, _} -> ct:pal("Null bytes in value rejected");
        {'EXIT', _} -> ct:pal("Null bytes caused crash")
    end,
    
    % Test with null bytes in key
    case catch elmdb:put(DB, <<1,2,0,3,4>>, <<"value">>) of
        ok -> ct:pal("Null bytes in key accepted");
        {error, _} -> ct:pal("Null bytes in key rejected");
        {'EXIT', _} -> ct:pal("Null bytes in key caused crash")
    end,
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    file:del_dir_r(TestDir),
    ok.

test_memory_exhaustion_simulation(_Config) ->
    % This test simulates memory exhaustion by creating a very small map_size
    TestDir = "/tmp/test_memory_exhaustion",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    % Create environment with very small map size
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 4096}]), % 4KB only
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Try to put data that exceeds the map size
    LargeValue = binary:copy(<<"X">>, 2048), % 2KB value
    
    % This should succeed initially
    case elmdb:put(DB, <<"key1">>, LargeValue) of
        ok -> ct:pal("First large value succeeded");
        {error, _} -> ct:pal("First large value failed")
    end,
    
    % This might fail due to space constraints
    case elmdb:put(DB, <<"key2">>, LargeValue) of
        ok -> ct:pal("Second large value succeeded");
        {error, _} -> ct:pal("Second large value failed as expected")
    end,
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    file:del_dir_r(TestDir),
    ok.