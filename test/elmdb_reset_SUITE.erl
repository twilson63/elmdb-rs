%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for reproducing "could not open database" errors after
%%% environment close/open cycles in elmdb NIF
%%% 
%%% This test suite specifically targets edge cases in environment
%%% and database lifecycle management that might cause issues with
%%% the global environment tracking and database handle invalidation.
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_reset_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% Common Test callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

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
        test_basic_close_reopen,
        test_close_reopen_with_data,
        test_env_close_vs_env_close_by_name,
        test_rapid_close_reopen_cycles,
        test_multiple_databases_close_reopen,
        test_database_after_env_close_by_name,
        test_database_after_env_close,
        test_concurrent_close_reopen,
        test_reopen_with_different_options,
        test_environment_already_open_error,
        test_database_operations_after_env_closed,
        test_multiple_env_handles_same_path,
        test_global_state_cleanup,
        test_stress_close_reopen,
        test_database_handle_after_forced_close,
        test_environment_tracking_edge_case,
        test_reproduce_database_open_error,
        test_global_state_inconsistency
    ].

%%====================================================================
%% Basic Close/Reopen Tests
%%====================================================================

test_basic_close_reopen(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Initial open
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    
    % Write some test data
    ok = elmdb:put(DB1, <<"key1">>, <<"value1">>),
    {ok, <<"value1">>} = elmdb:get(DB1, <<"key1">>),
    
    % Close database first, then environment
    ok = elmdb:db_close(DB1),
    ok = elmdb:env_close(Env1),
    
    % Wait a bit to ensure cleanup
    timer:sleep(100),
    
    % Re-open same path
    {ok, Env2} = elmdb:env_open(TestDir, []),
    
    % This is where the error might occur - try to open database again
    case elmdb:db_open(Env2, [create]) of
        {ok, DB2} ->
            % Verify data persisted
            case elmdb:get(DB2, <<"key1">>) of
                {ok, <<"value1">>} -> 
                    ct:pal("SUCCESS: Data persisted across close/reopen"),
                    ok = elmdb:db_close(DB2),
                    ok = elmdb:env_close(Env2);
                not_found ->
                    ct:pal("WARNING: Data not found after reopen"),
                    ok = elmdb:db_close(DB2),
                    ok = elmdb:env_close(Env2);
                Error ->
                    ct:fail("Unexpected get result: ~p", [Error])
            end;
        {error, Reason} ->
            ct:fail("Failed to open database after env reopen: ~p", [Reason])
    end.

test_close_reopen_with_data(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Create environment and database with substantial data
    {ok, Env1} = elmdb:env_open(TestDir, [{map_size, 10485760}]), % 10MB
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    
    % Write multiple key-value pairs
    TestData = [
        {<<"key1">>, <<"value1">>},
        {<<"key2">>, <<"value2">>},
        {<<"group/subkey1">>, <<"subvalue1">>},
        {<<"group/subkey2">>, <<"subvalue2">>},
        {<<"large_key">>, binary:copy(<<"X">>, 1000)} % 1KB value
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB1, Key, Value)
    end, TestData),
    
    % Flush to ensure data is written (commented out due to NIF loading issues)
    % ok = elmdb:flush(DB1),
    
    % Verify all data is accessible
    lists:foreach(fun({Key, ExpectedValue}) ->
        {ok, ExpectedValue} = elmdb:get(DB1, Key)
    end, TestData),
    
    % Close database first, then environment
    ok = elmdb:db_close(DB1),
    ok = elmdb:env_close(Env1),
    
    % Wait for cleanup
    timer:sleep(200),
    
    % Re-open environment
    {ok, Env2} = elmdb:env_open(TestDir, [{map_size, 10485760}]),
    
    % Critical test: try to open database after reopen
    case elmdb:db_open(Env2, []) of % Note: no create flag
        {ok, DB2} ->
            % Verify all data is still accessible
            lists:foreach(fun({Key, ExpectedValue}) ->
                case elmdb:get(DB2, Key) of
                    {ok, ExpectedValue} -> ok;
                    {ok, OtherValue} ->
                        ct:fail("Data mismatch for key ~p: expected ~p, got ~p", 
                               [Key, ExpectedValue, OtherValue]);
                    not_found ->
                        ct:fail("Key ~p not found after reopen", [Key]);
                    Error ->
                        ct:fail("Error getting key ~p: ~p", [Key, Error])
                end
            end, TestData),
            ok = elmdb:db_close(DB2),
            ok = elmdb:env_close(Env2);
        {error, Reason} ->
            ct:fail("Failed to open database after reopen: ~p", [Reason])
    end.

%%====================================================================
%% Environment Close Method Comparison Tests
%%====================================================================

test_env_close_vs_env_close_by_name(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Test env_close method
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"test_key_close">>, <<"test_value">>),
    ok = elmdb:env_close(Env1),
    
    timer:sleep(100),
    
    % Re-open and test
    {ok, Env2} = elmdb:env_open(TestDir, []),
    case elmdb:db_open(Env2, []) of
        {ok, DB2} ->
            {ok, <<"test_value">>} = elmdb:get(DB2, <<"test_key_close">>),
            ok = elmdb:db_close(DB2),
            ok = elmdb:env_close(Env2);
        Error1 ->
            ct:fail("env_close method failed: ~p", [Error1])
    end,
    
    timer:sleep(100),
    
    % Test env_close_by_name method
    {ok, Env3} = elmdb:env_open(TestDir, []),
    {ok, DB3} = elmdb:db_open(Env3, []),
    ok = elmdb:put(DB3, <<"test_key_close_by_name">>, <<"test_value2">>),
    ok = elmdb:env_close_by_name(TestDir),
    
    timer:sleep(100),
    
    % Re-open and test
    {ok, Env4} = elmdb:env_open(TestDir, []),
    case elmdb:db_open(Env4, []) of
        {ok, DB4} ->
            {ok, <<"test_value2">>} = elmdb:get(DB4, <<"test_key_close_by_name">>),
            ok = elmdb:db_close(DB4),
            ok = elmdb:env_close(Env4);
        Error2 ->
            ct:fail("env_close_by_name method failed: ~p", [Error2])
    end.

%%====================================================================
%% Rapid Cycling Tests
%%====================================================================

test_rapid_close_reopen_cycles(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Perform multiple rapid close/reopen cycles
    lists:foreach(fun(Cycle) ->
        ct:pal("Cycle ~p", [Cycle]),
        
        {ok, Env} = elmdb:env_open(TestDir, []),
        {ok, DB} = elmdb:db_open(Env, [create]),
        
        Key = list_to_binary("cycle_" ++ integer_to_list(Cycle)),
        Value = list_to_binary("value_" ++ integer_to_list(Cycle)),
        
        ok = elmdb:put(DB, Key, Value),
        {ok, Value} = elmdb:get(DB, Key),
        
        ok = elmdb:db_close(DB),
        ok = elmdb:env_close(Env),
        
        % Very short delay to stress test cleanup
        timer:sleep(10)
    end, lists:seq(1, 5)),
    
    % Final verification
    {ok, FinalEnv} = elmdb:env_open(TestDir, []),
    case elmdb:db_open(FinalEnv, []) of
        {ok, FinalDB} ->
            % Check that at least the last value exists
            {ok, <<"value_5">>} = elmdb:get(FinalDB, <<"cycle_5">>),
            ok = elmdb:db_close(FinalDB),
            ok = elmdb:env_close(FinalEnv);
        Error ->
            ct:fail("Final reopen failed after rapid cycles: ~p", [Error])
    end.

%%====================================================================
%% Multiple Database Tests
%%====================================================================

test_multiple_databases_close_reopen(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Open environment and multiple database handles
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    {ok, DB2} = elmdb:db_open(Env1, [create]),
    {ok, DB3} = elmdb:db_open(Env1, [create]),
    
    % Write data through different database handles
    ok = elmdb:put(DB1, <<"db1_key">>, <<"db1_value">>),
    ok = elmdb:put(DB2, <<"db2_key">>, <<"db2_value">>),
    ok = elmdb:put(DB3, <<"db3_key">>, <<"db3_value">>),
    
    % Verify data through all handles
    {ok, <<"db1_value">>} = elmdb:get(DB1, <<"db1_key">>),
    {ok, <<"db2_value">>} = elmdb:get(DB2, <<"db2_key">>),
    {ok, <<"db3_value">>} = elmdb:get(DB3, <<"db3_key">>),
    
    % Close database handles first, then environment (this should invalidate all database handles)
    ok = elmdb:db_close(DB1),
    ok = elmdb:db_close(DB2),
    ok = elmdb:db_close(DB3),
    ok = elmdb:env_close(Env1),
    
    timer:sleep(100),
    
    % Re-open environment
    {ok, Env2} = elmdb:env_open(TestDir, []),
    
    % Try to open multiple database handles again
    case elmdb:db_open(Env2, []) of
        {ok, NewDB1} ->
            case elmdb:db_open(Env2, []) of
                {ok, NewDB2} ->
                    % Verify all data is accessible through new handles
                    {ok, <<"db1_value">>} = elmdb:get(NewDB1, <<"db1_key">>),
                    {ok, <<"db2_value">>} = elmdb:get(NewDB2, <<"db2_key">>),
                    
                    % Test cross-handle access (should work since it's the same DB)
                    {ok, <<"db3_value">>} = elmdb:get(NewDB1, <<"db3_key">>),
                    
                    ok = elmdb:db_close(NewDB1),
                    ok = elmdb:db_close(NewDB2),
                    ok = elmdb:env_close(Env2);
                Error2 ->
                    ct:fail("Failed to open second DB handle after reopen: ~p", [Error2])
            end;
        Error1 ->
            ct:fail("Failed to open first DB handle after reopen: ~p", [Error1])
    end.

%%====================================================================
%% Database Handle Invalidation Tests
%%====================================================================

test_database_after_env_close_by_name(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    ok = elmdb:put(DB, <<"test_key">>, <<"test_value">>),
    
    % Close environment by name while keeping handles
    ok = elmdb:env_close_by_name(TestDir),
    
    % Try to use the database handle after env_close_by_name
    case elmdb:get(DB, <<"test_key">>) of
        {error, database_error, _} ->
            ct:pal("Expected error: database handle invalidated after env_close_by_name");
        {ok, _} ->
            ct:pal("WARNING: Database handle still works after env_close_by_name");
        Other ->
            ct:pal("Unexpected result: ~p", [Other])
    end,
    
    % Should be able to reopen
    {ok, NewEnv} = elmdb:env_open(TestDir, []),
    {ok, NewDB} = elmdb:db_open(NewEnv, []),
    {ok, <<"test_value">>} = elmdb:get(NewDB, <<"test_key">>),
    ok = elmdb:db_close(NewDB),
    ok = elmdb:env_close(NewEnv).

test_database_after_env_close(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    ok = elmdb:put(DB, <<"test_key">>, <<"test_value">>),
    
    % Close environment while keeping database handle
    ok = elmdb:env_close(Env),
    
    % Try to use the database handle after env_close
    case elmdb:get(DB, <<"test_key">>) of
        {error, database_error, _} ->
            ct:pal("Expected error: database handle invalidated after env_close");
        {ok, _} ->
            ct:pal("WARNING: Database handle still works after env_close");
        Other ->
            ct:pal("Unexpected result: ~p", [Other])
    end,
    
    % Should be able to reopen
    {ok, NewEnv} = elmdb:env_open(TestDir, []),
    {ok, NewDB} = elmdb:db_open(NewEnv, []),
    {ok, <<"test_value">>} = elmdb:get(NewDB, <<"test_key">>),
    ok = elmdb:db_close(NewDB),
    ok = elmdb:env_close(NewEnv).

%%====================================================================
%% Concurrency and Edge Case Tests
%%====================================================================

test_concurrent_close_reopen(Config) ->
    TestDir = ?config(test_dir, Config),
    
    Self = self(),
    
    % Spawn multiple processes that do close/reopen cycles
    Pids = lists:map(fun(N) ->
        spawn_link(fun() ->
            try
                {ok, Env} = elmdb:env_open(TestDir, []),
                {ok, DB} = elmdb:db_open(Env, [create]),
                
                Key = list_to_binary("proc_" ++ integer_to_list(N)),
                Value = list_to_binary("value_" ++ integer_to_list(N)),
                
                ok = elmdb:put(DB, Key, Value),
                {ok, Value} = elmdb:get(DB, Key),
                
                ok = elmdb:env_close(Env),
                
                timer:sleep(50 + N * 10), % Staggered timing
                
                {ok, Env2} = elmdb:env_open(TestDir, []),
                {ok, DB2} = elmdb:db_open(Env2, []),
                {ok, Value} = elmdb:get(DB2, Key),
                ok = elmdb:env_close(Env2),
                
                Self ! {success, N}
            catch
                Error:Reason:Stack ->
                    Self ! {error, N, {Error, Reason, Stack}}
            end
        end)
    end, lists:seq(1, 3)),
    
    % Wait for all processes to complete
    Results = lists:map(fun(N) ->
        receive
            {success, N} -> success;
            {error, N, Error} -> {error, Error}
        after 10000 ->
            timeout
        end
    end, lists:seq(1, 3)),
    
    % Check results
    case lists:all(fun(R) -> R =:= success end, Results) of
        true ->
            ct:pal("All concurrent operations succeeded");
        false ->
            Errors = [R || R <- Results, R =/= success],
            ct:fail("Some concurrent operations failed: ~p", [Errors])
    end,
    
    % Clean up any remaining processes
    lists:foreach(fun(Pid) ->
        case is_process_alive(Pid) of
            true -> exit(Pid, kill);
            false -> ok
        end
    end, Pids).

%%====================================================================
%% Option Variation Tests
%%====================================================================

test_reopen_with_different_options(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Open with initial options
    {ok, Env1} = elmdb:env_open(TestDir, [{map_size, 1048576}]), % 1MB
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"test_key">>, <<"test_value">>),
    ok = elmdb:env_close(Env1),
    
    % Reopen with different options
    {ok, Env2} = elmdb:env_open(TestDir, [{map_size, 10485760}]), % 10MB
    case elmdb:db_open(Env2, []) of
        {ok, DB2} ->
            {ok, <<"test_value">>} = elmdb:get(DB2, <<"test_key">>),
            ok = elmdb:db_close(DB2),
            ok = elmdb:env_close(Env2);
        Error ->
            ct:fail("Failed to reopen with different options: ~p", [Error])
    end,
    
    % Reopen with additional flags
    {ok, Env3} = elmdb:env_open(TestDir, [no_sync, {map_size, 10485760}]),
    case elmdb:db_open(Env3, [create]) of
        {ok, DB3} ->
            {ok, <<"test_value">>} = elmdb:get(DB3, <<"test_key">>),
            ok = elmdb:db_close(DB3),
            ok = elmdb:env_close(Env3);
        Error2 ->
            ct:fail("Failed to reopen with additional flags: ~p", [Error2])
    end.

%%====================================================================
%% Error Condition Tests
%%====================================================================

test_environment_already_open_error(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Open environment
    {ok, Env1} = elmdb:env_open(TestDir, []),
    
    % Try to open the same environment again
    case elmdb:env_open(TestDir, []) of
        {error, already_open} ->
            ct:pal("Correctly detected already open environment");
        {ok, Env2} ->
            % This might be allowed in some implementations
            ct:pal("WARNING: Second environment handle created for same path"),
            ok = elmdb:env_close(Env2);
        Error ->
            ct:pal("Unexpected error: ~p", [Error])
    end,
    
    ok = elmdb:env_close(Env1),
    
    % Now it should work
    {ok, Env3} = elmdb:env_open(TestDir, []),
    ok = elmdb:env_close(Env3).

test_database_operations_after_env_closed(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    ok = elmdb:put(DB, <<"key1">>, <<"value1">>),
    {ok, <<"value1">>} = elmdb:get(DB, <<"key1">>),
    
    % Close environment
    ok = elmdb:env_close(Env),
    
    % Try various database operations after environment is closed
    Operations = [
        fun() -> elmdb:put(DB, <<"key2">>, <<"value2">>) end,
        fun() -> elmdb:get(DB, <<"key1">>) end,
        % fun() -> elmdb:flush(DB) end,
        fun() -> elmdb:list(DB, <<"">>) end
    ],
    
    lists:foreach(fun(Op) ->
        case Op() of
            {error, database_error, _} ->
                ct:pal("Operation correctly failed with database_error");
            ok ->
                ct:pal("WARNING: Operation succeeded after env closed");
            {ok, _} ->
                ct:pal("WARNING: Operation succeeded after env closed");
            Other ->
                ct:pal("Unexpected result: ~p", [Other])
        end
    end, Operations).

%%====================================================================
%% Global State Management Tests
%%====================================================================

test_multiple_env_handles_same_path(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % This test checks the global environment tracking
    {ok, Env1} = elmdb:env_open(TestDir, []),
    
    % Try to open same path again (should fail with already_open)
    case elmdb:env_open(TestDir, []) of
        {error, already_open} ->
            ct:pal("Global tracking working: detected already open");
        {ok, Env2} ->
            ct:pal("WARNING: Multiple handles for same path allowed"),
            ok = elmdb:env_close(Env2);
        Error ->
            ct:fail("Unexpected error: ~p", [Error])
    end,
    
    ok = elmdb:env_close(Env1),
    
    % Should be able to open again after close
    {ok, Env3} = elmdb:env_open(TestDir, []),
    ok = elmdb:env_close(Env3).

test_global_state_cleanup(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Test that global state is properly cleaned up
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"cleanup_test">>, <<"value">>),
    
    % Close with env_close
    ok = elmdb:env_close(Env1),
    
    % Should be able to reopen immediately
    {ok, Env2} = elmdb:env_open(TestDir, []),
    {ok, DB2} = elmdb:db_open(Env2, []),
    {ok, <<"value">>} = elmdb:get(DB2, <<"cleanup_test">>),
    
    % Close with env_close_by_name
    ok = elmdb:env_close_by_name(TestDir),
    
    % Should be able to reopen immediately
    {ok, Env3} = elmdb:env_open(TestDir, []),
    {ok, DB3} = elmdb:db_open(Env3, []),
    {ok, <<"value">>} = elmdb:get(DB3, <<"cleanup_test">>),
    
    ok = elmdb:env_close(Env3).

%%====================================================================
%% Stress and Edge Case Tests
%%====================================================================

test_stress_close_reopen(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Stress test with many rapid close/reopen cycles
    % This might expose race conditions in global environment tracking
    lists:foreach(fun(I) ->
        {ok, Env} = elmdb:env_open(TestDir, []),
        {ok, DB} = elmdb:db_open(Env, [create]),
        
        % Write a small amount of data
        Key = <<"stress_", (integer_to_binary(I))/binary>>,
        ok = elmdb:put(DB, Key, <<"value">>),
        
        % Immediately close
        ok = elmdb:db_close(DB),
        ok = elmdb:env_close(Env),
        
        % Very brief delay (might expose race conditions)
        timer:sleep(1)
    end, lists:seq(1, 20)),
    
    % Final verification
    {ok, FinalEnv} = elmdb:env_open(TestDir, []),
    {ok, FinalDB} = elmdb:db_open(FinalEnv, []),
    
    % Check that at least some data exists
    case elmdb:get(FinalDB, <<"stress_20">>) of
        {ok, <<"value">>} ->
            ct:pal("Stress test passed: data persisted");
        not_found ->
            ct:pal("WARNING: Final key not found after stress test");
        Error ->
            ct:fail("Error in final verification: ~p", [Error])
    end,
    
    ok = elmdb:db_close(FinalDB),
    ok = elmdb:env_close(FinalEnv).

test_database_handle_after_forced_close(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Test what happens to database handles when environment is forcibly closed
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env, [create]),
    {ok, DB2} = elmdb:db_open(Env, [create]),
    
    % Write some data through both handles
    ok = elmdb:put(DB1, <<"key1">>, <<"value1">>),
    ok = elmdb:put(DB2, <<"key2">>, <<"value2">>),
    
    % Verify data is accessible
    {ok, <<"value1">>} = elmdb:get(DB1, <<"key1">>),
    {ok, <<"value2">>} = elmdb:get(DB2, <<"key2">>),
    
    % Force close environment by name (might not properly invalidate handles)
    ok = elmdb:env_close_by_name(TestDir),
    
    % Now try to use the database handles
    Results = [
        try elmdb:get(DB1, <<"key1">>) of
            {error, database_error, _} -> db1_properly_invalidated;
            {ok, _} -> db1_still_works;
            Other -> {db1_unexpected, Other}
        catch
            _:_ -> db1_exception
        end,
        try elmdb:get(DB2, <<"key2">>) of
            {error, database_error, _} -> db2_properly_invalidated;
            {ok, _} -> db2_still_works;
            Other2 -> {db2_unexpected, Other2}
        catch
            _:_ -> db2_exception
        end
    ],
    
    ct:pal("Database handle results after env_close_by_name: ~p", [Results]),
    
    % Should be able to reopen
    {ok, NewEnv} = elmdb:env_open(TestDir, []),
    {ok, NewDB} = elmdb:db_open(NewEnv, []),
    
    % Verify data persisted
    {ok, <<"value1">>} = elmdb:get(NewDB, <<"key1">>),
    {ok, <<"value2">>} = elmdb:get(NewDB, <<"key2">>),
    
    ok = elmdb:db_close(NewDB),
    ok = elmdb:env_close(NewEnv).

test_environment_tracking_edge_case(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Test edge case: what happens if we try operations in specific orders
    % that might confuse the global environment tracking
    
    % Open environment
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"tracking_test">>, <<"initial">>),
    
    % Close by name first
    ok = elmdb:env_close_by_name(TestDir),
    
    % Now try to close the original handle
    case elmdb:env_close(Env1) of
        ok ->
            ct:pal("Both close methods succeeded");
        Error ->
            ct:pal("Second close failed: ~p", [Error])
    end,
    
    % Try to reopen immediately
    case elmdb:env_open(TestDir, []) of
        {ok, Env2} ->
            case elmdb:db_open(Env2, []) of
                {ok, DB2} ->
                    case elmdb:get(DB2, <<"tracking_test">>) of
                        {ok, <<"initial">>} ->
                            ct:pal("Data persisted through edge case");
                        not_found ->
                            ct:pal("WARNING: Data lost in edge case");
                        Error2 ->
                            ct:fail("Error reading after edge case: ~p", [Error2])
                    end,
                    ok = elmdb:env_close(Env2);
                DbError ->
                    ct:fail("Failed to open DB after edge case: ~p", [DbError])
            end;
        EnvError ->
            ct:fail("Failed to reopen env after edge case: ~p", [EnvError])
    end.

test_reproduce_database_open_error(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % This test specifically tries to reproduce the "could not open database" error
    % by creating conditions where global state gets confused
    
    % Step 1: Normal operation
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"initial">>, <<"data">>),
    
    % Step 2: Keep database handle but close environment by name
    % This might leave the global state inconsistent
    ok = elmdb:env_close_by_name(TestDir),
    
    % Step 3: Try to open the same environment path again
    % The global tracking should allow this since we removed it from the map
    case elmdb:env_open(TestDir, []) of
        {ok, Env2} ->
            ct:pal("Successfully reopened environment"),
            
            % Step 4: Now try to open a database - this is where error might occur
            case elmdb:db_open(Env2, []) of
                {ok, DB2} ->
                    ct:pal("Successfully opened database after reopen"),
                    
                    % Verify data persistence
                    case elmdb:get(DB2, <<"initial">>) of
                        {ok, <<"data">>} ->
                            ct:pal("Data persisted correctly");
                        not_found ->
                            ct:pal("WARNING: Data lost");
                        Error ->
                            ct:pal("Error reading data: ~p", [Error])
                    end,
                    ok = elmdb:env_close(Env2);
                    
                {error, Reason} ->
                    ct:pal("REPRODUCED ERROR: Failed to open database: ~p", [Reason]),
                    % This might be the "could not open database" error we're looking for
                    ct:fail("Database open failed after env reopen: ~p", [Reason])
            end;
            
        {error, already_open} ->
            ct:pal("Environment tracking inconsistency: still marked as open"),
            ct:fail("Environment still marked as open after close_by_name");
            
        {error, OtherError} ->
            ct:fail("Unexpected error reopening environment: ~p", [OtherError])
    end,
    
    % Clean up: try to use the original database handle
    case elmdb:get(DB1, <<"initial">>) of
        {error, database_error, _} ->
            ct:pal("Original DB handle properly invalidated");
        {ok, _} ->
            ct:pal("WARNING: Original DB handle still works");
        Other ->
            ct:pal("Original DB handle result: ~p", [Other])
    end.

test_global_state_inconsistency(Config) ->
    TestDir = ?config(test_dir, Config),
    
    % Test for global state inconsistencies that might cause database open errors
    
    % Create multiple handles and close in different ways to confuse global state
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    {ok, DB2} = elmdb:db_open(Env1, [create]),
    
    % Write test data
    ok = elmdb:put(DB1, <<"key1">>, <<"value1">>),
    ok = elmdb:put(DB2, <<"key2">>, <<"value2">>),
    
    % Close environment handle directly
    ok = elmdb:env_close(Env1),
    
    % Now the database handles should be invalidated, but let's check
    DB1Result = try elmdb:get(DB1, <<"key1">>) of
        R1 -> R1
    catch
        _:_ -> exception
    end,
    
    DB2Result = try elmdb:get(DB2, <<"key2">>) of
        R2 -> R2
    catch
        _:_ -> exception
    end,
    
    ct:pal("DB handles after env_close: DB1=~p, DB2=~p", [DB1Result, DB2Result]),
    
    % Try to reopen - this should work
    case elmdb:env_open(TestDir, []) of
        {ok, NewEnv} ->
            % This is the critical test - can we open a database?
            case elmdb:db_open(NewEnv, []) of
                {ok, NewDB} ->
                    % Verify data
                    {ok, <<"value1">>} = elmdb:get(NewDB, <<"key1">>),
                    {ok, <<"value2">>} = elmdb:get(NewDB, <<"key2">>),
                    ct:pal("Successfully reopened and verified data"),
                    ok = elmdb:env_close(NewEnv);
                    
                {error, DbError} ->
                    ct:fail("FOUND THE BUG: Cannot open database after env_close/reopen: ~p", [DbError])
            end;
            
        {error, EnvError} ->
            ct:fail("Cannot reopen environment: ~p", [EnvError])
    end.