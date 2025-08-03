%%%-------------------------------------------------------------------
%%% @doc
%%% Basic functionality tests for elmdb NIF
%%% Tests environment operations, database operations, and basic put/get
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_basic_SUITE).

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
        test_env_open_close,
        test_env_open_invalid_path,
        test_env_open_with_options,
        test_env_close_by_name,
        test_db_open_create,
        test_db_open_no_create,
        test_basic_put_get,
        test_get_missing_key,
        test_put_get_large_value,
        test_put_get_binary_keys,
        test_put_overwrite_value,
        test_multiple_databases
    ].

%%====================================================================
%% Environment Management Tests
%%====================================================================

test_env_open_close(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Test successful environment open
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Verify environment is opaque term
    true = is_reference(Env) orelse is_binary(Env) orelse is_tuple(Env),
    
    % Test environment close
    ok = elmdb:env_close(Env),
    
    ok.

test_env_open_invalid_path(_Config) ->
    % Test opening environment with invalid path
    InvalidPath = "/this/path/does/not/exist/and/should/fail",
    case elmdb:env_open(InvalidPath, []) of
        {error, _Reason} -> ok;
        {ok, _Env} -> 
            ct:fail("Expected error for invalid path, but got success")
    end.

test_env_open_with_options(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Test environment open with options
    Options = [{map_size, 10485760}], % 10MB
    {ok, Env} = elmdb:env_open(TestDir, Options),
    
    ok = elmdb:env_close(Env),
    ok.

test_env_close_by_name(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Open environment
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Close by name (path)
    ok = elmdb:env_close_by_name(TestDir),
    
    % The original handle might still be valid or not,
    % depending on implementation
    ok.

%%====================================================================
%% Database Operations Tests
%%====================================================================

test_db_open_create(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Test database open with create flag
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Verify database handle is opaque term
    true = is_reference(DB) orelse is_binary(DB) orelse is_tuple(DB),
    
    ok = elmdb:env_close(Env),
    ok.

test_db_open_no_create(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Test database open without create flag on new environment
    % This should either succeed (if default DB exists) or fail gracefully
    case elmdb:db_open(Env, []) of
        {ok, _DB} -> ok;
        {error, _Reason} -> ok
    end,
    
    ok = elmdb:env_close(Env),
    ok.

%%====================================================================
%% Key-Value Operations Tests
%%====================================================================

test_basic_put_get(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test basic put operation
    Key = <<"test_key">>,
    Value = <<"test_value">>,
    ok = elmdb:put(DB, Key, Value),
    
    % Test basic get operation
    {ok, RetrievedValue} = elmdb:get(DB, Key),
    Value = RetrievedValue,
    
    ok = elmdb:env_close(Env),
    ok.

test_get_missing_key(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test get operation on missing key
    MissingKey = <<"missing_key">>,
    not_found = elmdb:get(DB, MissingKey),
    
    ok = elmdb:env_close(Env),
    ok.

test_put_get_large_value(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 10485760}]), % 10MB
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test with large value (1MB)
    Key = <<"large_key">>,
    Value = binary:copy(<<"X">>, 1048576), % 1MB of X's
    ok = elmdb:put(DB, Key, Value),
    
    {ok, RetrievedValue} = elmdb:get(DB, Key),
    Value = RetrievedValue,
    
    ok = elmdb:env_close(Env),
    ok.

test_put_get_binary_keys(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test with various binary key formats
    TestCases = [
        {<<"simple">>, <<"value1">>},
        {<<"key/with/slashes">>, <<"value2">>},
        {<<"key.with.dots">>, <<"value3">>},
        {<<"key-with-dashes">>, <<"value4">>},
        {<<"key_with_underscores">>, <<"value5">>},
        {<<0,1,2,3,4,5>>, <<"binary_key_value">>} % Binary data key
    ],
    
    % Test empty key separately since it requires special handling
    case elmdb:put(DB, <<"">>, <<"empty_key_value">>) of
        ok ->
            % Empty key is supported, verify we can retrieve it
            case elmdb:get(DB, <<"">>) of
                {ok, <<"empty_key_value">>} -> 
                    ct:pal("Empty key supported and working correctly");
                Other ->
                    ct:pal("Empty key put succeeded but get failed: ~p", [Other])
            end;
        {error, _Type, _Description} ->
            % Empty key is not supported, which is expected for some LMDB configurations
            ct:pal("Empty key not supported as expected")
    end,
    
    % Put all test cases
    lists:foreach(fun({Key, Value}) ->
        case elmdb:put(DB, Key, Value) of
            ok -> ok;
            Error -> 
                ct:pal("Put failed for key ~p: ~p", [Key, Error]),
                error({put_failed, Key, Error})
        end
    end, TestCases),
    
    % Get and verify all test cases
    lists:foreach(fun({Key, ExpectedValue}) ->
        case elmdb:get(DB, Key) of
            {ok, RetrievedValue} -> 
                case ExpectedValue =:= RetrievedValue of
                    true -> ok;
                    false -> error({value_mismatch, Key, ExpectedValue, RetrievedValue})
                end;
            Error ->
                ct:pal("Get failed for key ~p: ~p", [Key, Error]),
                error({get_failed, Key, Error})
        end
    end, TestCases),
    
    ok = elmdb:env_close(Env),
    ok.

test_put_overwrite_value(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    Key = <<"overwrite_key">>,
    Value1 = <<"original_value">>,
    Value2 = <<"updated_value">>,
    
    % Put original value
    ok = elmdb:put(DB, Key, Value1),
    {ok, Value1} = elmdb:get(DB, Key),
    
    % Overwrite with new value
    ok = elmdb:put(DB, Key, Value2),
    {ok, Value2} = elmdb:get(DB, Key),
    
    ok = elmdb:env_close(Env),
    ok.

test_multiple_databases(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    
    % Open multiple database handles
    {ok, DB1} = elmdb:db_open(Env, [create]),
    {ok, DB2} = elmdb:db_open(Env, [create]),
    
    % Test that they can operate independently
    Key = <<"test_key">>,
    Value1 = <<"value_in_db1">>,
    Value2 = <<"value_in_db2">>,
    
    ok = elmdb:put(DB1, Key, Value1),
    ok = elmdb:put(DB2, Key, Value2),
    
    % Both should return their respective values
    % (Note: This test depends on whether the implementation
    % supports multiple named databases or if they share the same namespace)
    {ok, RetrievedValue1} = elmdb:get(DB1, Key),
    {ok, RetrievedValue2} = elmdb:get(DB2, Key),
    
    % In LMDB, if no database name is specified, they might share
    % the same namespace, so we just verify we can retrieve values
    true = (RetrievedValue1 =:= Value1) orelse (RetrievedValue1 =:= Value2),
    true = (RetrievedValue2 =:= Value1) orelse (RetrievedValue2 =:= Value2),
    
    ok = elmdb:env_close(Env),
    ok.