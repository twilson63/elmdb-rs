%%%-------------------------------------------------------------------
%%% @doc
%%% Focused test script to reproduce the "could not open database" error
%%% 
%%% This script creates a simple test that:
%%% 1. Opens environment and database
%%% 2. Writes data
%%% 3. Closes environment  
%%% 4. Immediately reopens environment with same path
%%% 5. Tries to open database again (where error occurs)
%%% 6. Runs this in a loop to trigger the error
%%%
%%% The goal is to isolate and reproduce the exact conditions that cause
%%% the database open failure after environment close/reopen cycles.
%%% @end
%%%-------------------------------------------------------------------
-module(test_reset_issue).

-export([run/0, run_single_test/1, run_loop/1, run_with_delay/2, test_close_methods/0, test_rapid_cycling/0, test_same_path/0, test_env_close_by_name_issue/0, test_aggressive_race_condition/0]).

-define(TEST_DIR, "/tmp/elmdb_reset_test").

%% @doc Main entry point - run the test
run() ->
    io:format("Starting elmdb reset issue test...~n"),
    
    % Clean up any existing test directory
    cleanup_test_dir(),
    
    % Ensure test directory exists
    ok = filelib:ensure_dir(?TEST_DIR ++ "/"),
    
    io:format("Running single test...~n"),
    case run_single_test(?TEST_DIR) of
        ok ->
            io:format("Single test passed. Running loop test...~n"),
            run_loop(10);
        Error ->
            io:format("Single test failed: ~p~n", [Error]),
            Error
    end.

%% @doc Run a single test cycle to check for the error
run_single_test(TestDir) ->
    try
        io:format("  Step 1: Opening environment at ~s~n", [TestDir]),
        {ok, Env} = elmdb:env_open(TestDir, [{map_size, 10485760}]), % 10MB
        
        io:format("  Step 2: Opening database~n"),
        {ok, DB} = elmdb:db_open(Env, [create]),
        
        io:format("  Step 3: Writing test data~n"),
        ok = elmdb:put(DB, <<"test_key">>, <<"test_value">>),
        
        io:format("  Step 4: Reading test data to verify~n"),
        {ok, <<"test_value">>} = elmdb:get(DB, <<"test_key">>),
        
        io:format("  Step 5: Closing environment~n"),
        ok = elmdb:env_close(Env),
        
        io:format("  Step 6: Waiting brief moment for cleanup...~n"),
        timer:sleep(50), % Brief delay for cleanup
        
        io:format("  Step 7: Reopening environment with same path~n"),
        {ok, Env2} = elmdb:env_open(TestDir, [{map_size, 10485760}]),
        
        io:format("  Step 8: Trying to open database again (CRITICAL STEP)~n"),
        case elmdb:db_open(Env2, []) of % Note: no create flag on reopen
            {ok, DB2} ->
                io:format("  SUCCESS: Database opened successfully~n"),
                
                io:format("  Step 9: Verifying data persistence~n"),
                case elmdb:get(DB2, <<"test_key">>) of
                    {ok, <<"test_value">>} ->
                        io:format("  SUCCESS: Data persisted correctly~n");
                    not_found ->
                        io:format("  WARNING: Data not found (this is OK for LMDB)~n");
                    DataError ->
                        io:format("  ERROR: Unexpected data read error: ~p~n", [DataError])
                end,
                
                io:format("  Step 10: Closing second environment~n"),
                ok = elmdb:env_close(Env2),
                ok;
                
            {error, Reason} ->
                io:format("  *** REPRODUCED ERROR ***: Failed to open database: ~p~n", [Reason]),
                % Don't fail here, just return the error for analysis
                ok = elmdb:env_close(Env2),
                {database_open_error, Reason}
        end
        
    catch
        ErrorType:ErrorReason:Stack ->
            io:format("  EXCEPTION in single test: ~p:~p~n", [ErrorType, ErrorReason]),
            io:format("  Stack: ~p~n", [Stack]),
            {exception, ErrorType, ErrorReason}
    end.

%% @doc Run the test in a loop to increase chances of triggering the error
run_loop(NumIterations) ->
    io:format("Running loop test with ~p iterations...~n", [NumIterations]),
    
    Results = lists:map(fun(I) ->
        io:format("--- Iteration ~p/~p ---~n", [I, NumIterations]),
        
        % Use iteration-specific directory to avoid conflicts
        IterDir = ?TEST_DIR ++ "_" ++ integer_to_list(I),
        ok = filelib:ensure_dir(IterDir ++ "/"),
        
        Result = run_single_test(IterDir),
        
        % Brief delay between iterations
        timer:sleep(10),
        
        % Clean up iteration directory
        case file:del_dir_r(IterDir) of
            ok -> ok;
            {error, enoent} -> ok;
            DelError -> 
                io:format("  WARNING: Failed to clean up ~s: ~p~n", [IterDir, DelError])
        end,
        
        {I, Result}
    end, lists:seq(1, NumIterations)),
    
    % Analyze results
    Errors = [{I, R} || {I, R} <- Results, R =/= ok],
    
    case Errors of
        [] ->
            io:format("~nALL TESTS PASSED: No errors found in ~p iterations~n", [NumIterations]),
            ok;
        _ ->
            io:format("~nERRORS FOUND in loop test:~n"),
            lists:foreach(fun({I, Error}) ->
                io:format("  Iteration ~p: ~p~n", [I, Error])
            end, Errors),
            {errors_found, length(Errors), Errors}
    end.

%% @doc Run test with specific delay between close and reopen
run_with_delay(DelayMs, NumIterations) ->
    io:format("Running test with ~pms delay, ~p iterations...~n", [DelayMs, NumIterations]),
    
    Results = lists:map(fun(I) ->
        io:format("--- Iteration ~p with ~pms delay ---~n", [I, DelayMs]),
        
        try
            % Use iteration-specific directory
            IterDir = ?TEST_DIR ++ "_delay_" ++ integer_to_list(I),
            ok = filelib:ensure_dir(IterDir ++ "/"),
            
            % Open, write, close
            {ok, Env} = elmdb:env_open(IterDir, [{map_size, 10485760}]),
            {ok, DB} = elmdb:db_open(Env, [create]),
            ok = elmdb:put(DB, <<"key">>, <<"value">>),
            ok = elmdb:env_close(Env),
            
            % Wait specified delay
            timer:sleep(DelayMs),
            
            % Reopen and test
            {ok, Env2} = elmdb:env_open(IterDir, [{map_size, 10485760}]),
            Result = case elmdb:db_open(Env2, []) of
                {ok, _DB2} ->
                    ok = elmdb:env_close(Env2),
                    ok;
                {error, Reason} ->
                    ok = elmdb:env_close(Env2),
                    {database_open_error, Reason}
            end,
            
            % Cleanup
            case file:del_dir_r(IterDir) of
                ok -> ok;
                {error, enoent} -> ok;
                _ -> ok
            end,
            
            {I, Result}
            
        catch
            ErrorType:ErrorReason:_Stack ->
                {I, {exception, ErrorType, ErrorReason}}
        end
    end, lists:seq(1, NumIterations)),
    
    % Report results
    Errors = [{I, R} || {I, R} <- Results, R =/= ok],
    case Errors of
        [] ->
            io:format("~nNo errors with ~pms delay~n", [DelayMs]);
        _ ->
            io:format("~nErrors with ~pms delay:~n", [DelayMs]),
            lists:foreach(fun({I, E}) ->
                io:format("  Iteration ~p: ~p~n", [I, E])
            end, Errors)
    end,
    
    {DelayMs, length(Errors), Errors}.

%% @doc Clean up test directory
cleanup_test_dir() ->
    case file:del_dir_r(?TEST_DIR) of
        ok -> 
            io:format("Cleaned up existing test directory~n");
        {error, enoent} -> 
            ok; % Directory doesn't exist, that's fine
        {error, Reason} -> 
            io:format("WARNING: Could not clean up test dir: ~p~n", [Reason])
    end.

%% Additional debugging functions

%% @doc Test specifically for env_close_by_name vs env_close
test_close_methods() ->
    io:format("Testing different close methods...~n"),
    
    % Test env_close method
    TestDir1 = ?TEST_DIR ++ "_close_test",
    ok = filelib:ensure_dir(TestDir1 ++ "/"),
    
    {ok, Env1} = elmdb:env_open(TestDir1, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"key1">>, <<"value1">>),
    ok = elmdb:env_close(Env1), % Use env_close
    
    timer:sleep(50),
    
    {ok, Env1b} = elmdb:env_open(TestDir1, []),
    Result1 = case elmdb:db_open(Env1b, []) of
        {ok, _} -> 
            ok = elmdb:env_close(Env1b),
            success;
        {error, R} -> 
            ok = elmdb:env_close(Env1b),
            {failed, R}
    end,
    
    file:del_dir_r(TestDir1),
    io:format("env_close result: ~p~n", [Result1]),
    
    % Test env_close_by_name method
    TestDir2 = ?TEST_DIR ++ "_close_by_name_test",
    ok = filelib:ensure_dir(TestDir2 ++ "/"),
    
    {ok, Env2} = elmdb:env_open(TestDir2, []),
    {ok, DB2} = elmdb:db_open(Env2, [create]),
    ok = elmdb:put(DB2, <<"key2">>, <<"value2">>),
    ok = elmdb:env_close_by_name(TestDir2), % Use env_close_by_name
    
    timer:sleep(50),
    
    {ok, Env2b} = elmdb:env_open(TestDir2, []),
    Result2 = case elmdb:db_open(Env2b, []) of
        {ok, _} -> 
            ok = elmdb:env_close(Env2b),
            success;
        {error, DbError} -> 
            ok = elmdb:env_close(Env2b),
            {failed, DbError}
    end,
    
    file:del_dir_r(TestDir2),
    io:format("env_close_by_name result: ~p~n", [Result2]),
    
    {Result1, Result2}.

%% @doc Test with rapid cycling to stress the global state management
test_rapid_cycling() ->
    io:format("Testing rapid cycling...~n"),
    
    TestDir = ?TEST_DIR ++ "_rapid",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    % Do very rapid open/close cycles
    Results = lists:map(fun(I) ->
        {ok, Env} = elmdb:env_open(TestDir, []),
        {ok, DB} = elmdb:db_open(Env, [create]),
        ok = elmdb:put(DB, <<"rapid_key">>, <<"rapid_value">>),
        ok = elmdb:env_close(Env),
        % No delay - immediately next iteration
        I
    end, lists:seq(1, 20)),
    
    % Now try to reopen
    FinalResult = case elmdb:env_open(TestDir, []) of
        {ok, FinalEnv} ->
            case elmdb:db_open(FinalEnv, []) of
                {ok, FinalDB} ->
                    ReadResult = elmdb:get(FinalDB, <<"rapid_key">>),
                    ok = elmdb:env_close(FinalEnv),
                    {success, ReadResult};
                {error, R} ->
                    ok = elmdb:env_close(FinalEnv),
                    {db_open_failed, R}
            end;
        {error, EnvError} ->
            {env_open_failed, EnvError}
    end,
    
    file:del_dir_r(TestDir),
    io:format("Rapid cycling result: ~p~n", [FinalResult]),
    FinalResult.

%% @doc Test using the same path repeatedly (most likely to trigger the error)
test_same_path() ->
    io:format("Testing same path reuse...~n"),
    
    TestDir = ?TEST_DIR ++ "_same_path",
    cleanup_dir(TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    % First test: Normal open/close cycle
    io:format("1. Initial open/close cycle~n"),
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"cycle1">>, <<"value1">>),
    ok = elmdb:env_close(Env1),
    
    % Second test: Reopen same path immediately
    io:format("2. Immediate reopen (no delay)~n"),
    {ok, Env2} = elmdb:env_open(TestDir, []),
    Result1 = case elmdb:db_open(Env2, []) of
        {ok, DB2} ->
            ReadResult = elmdb:get(DB2, <<"cycle1">>),
            ok = elmdb:env_close(Env2),
            {success, ReadResult};
        {error, R} ->
            ok = elmdb:env_close(Env2),
            {failed, R}
    end,
    io:format("   Result: ~p~n", [Result1]),
    
    % Third test: Open and close multiple times with same path
    io:format("3. Multiple rapid cycles on same path~n"),
    lists:foreach(fun(I) ->
        {ok, Env} = elmdb:env_open(TestDir, []),
        {ok, DB} = elmdb:db_open(Env, []),
        Key = <<"multi_", (integer_to_binary(I))/binary>>,
        ok = elmdb:put(DB, Key, <<"value">>),
        ok = elmdb:env_close(Env)
    end, lists:seq(1, 5)),
    
    % Fourth test: Final open to see if it works
    io:format("4. Final verification~n"),
    {ok, FinalEnv} = elmdb:env_open(TestDir, []),
    FinalResult = case elmdb:db_open(FinalEnv, []) of
        {ok, FinalDB} ->
            % Check if any data exists
            TestKeys = [<<"cycle1">>, <<"multi_1">>, <<"multi_5">>],
            Results = [elmdb:get(FinalDB, K) || K <- TestKeys],
            ok = elmdb:env_close(FinalEnv),
            {success, Results};
        {error, DbOpenError} ->
            ok = elmdb:env_close(FinalEnv),
            {final_db_open_failed, DbOpenError}
    end,
    
    io:format("   Final result: ~p~n", [FinalResult]),
    cleanup_dir(TestDir),
    FinalResult.

%% @doc Test that specifically tries to use env_close_by_name to trigger the issue
test_env_close_by_name_issue() ->
    io:format("Testing env_close_by_name specific issue...~n"),
    
    TestDir = ?TEST_DIR ++ "_close_by_name_issue",
    cleanup_dir(TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    % Step 1: Open environment and database
    io:format("1. Opening environment and database~n"),
    {ok, Env1} = elmdb:env_open(TestDir, []),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"test">>, <<"data">>),
    
    % Step 2: Close by name while keeping handles
    io:format("2. Closing environment by name (keeping handles)~n"),
    ok = elmdb:env_close_by_name(TestDir),
    
    % Step 3: Try to reopen immediately (this should work)
    io:format("3. Reopening environment with same path~n"),
    case elmdb:env_open(TestDir, []) of
        {ok, Env2} ->
            io:format("   Environment reopened successfully~n"),
            
            % Step 4: Try to open database (this might fail)
            io:format("4. Trying to open database on reopened environment~n"),
            case elmdb:db_open(Env2, []) of
                {ok, DB2} ->
                    io:format("   SUCCESS: Database opened~n"),
                    
                    % Test if we can read data
                    case elmdb:get(DB2, <<"test">>) of
                        {ok, <<"data">>} ->
                            io:format("   Data read successfully~n");
                        not_found ->
                            io:format("   Data not found (OK for LMDB)~n");
                        ReadError ->
                            io:format("   Read error: ~p~n", [ReadError])
                    end,
                    
                    ok = elmdb:env_close(Env2),
                    success;
                    
                {error, DbError} ->
                    io:format("   *** DATABASE OPEN FAILED ***: ~p~n", [DbError]),
                    ok = elmdb:env_close(Env2),
                    {db_open_failed, DbError}
            end;
            
        {error, already_open} ->
            io:format("   Environment still marked as open (global state issue)~n"),
            {env_still_open_error, already_open};
            
        {error, EnvError} ->
            io:format("   Environment reopen failed: ~p~n", [EnvError]),
            {env_reopen_failed, EnvError}
    end,
    
    % Step 5: Test what happens to the original handles
    io:format("5. Testing original handles after close_by_name~n"),
    DB1Result = try elmdb:get(DB1, <<"test">>) of
        {error, database_error, _} -> invalidated_properly;
        {ok, _} -> still_works;
        Other -> {unexpected, Other}
    catch
        _:_ -> exception_thrown
    end,
    io:format("   Original DB handle result: ~p~n", [DB1Result]),
    
    cleanup_dir(TestDir),
    ok.

%% @doc Aggressive test to try to trigger race conditions
test_aggressive_race_condition() ->
    io:format("Testing aggressive race conditions...~n"),
    
    TestDir = ?TEST_DIR ++ "_race",
    cleanup_dir(TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    % Test rapid close/reopen with zero delay
    lists:foreach(fun(I) ->
        io:format("  Rapid cycle ~p~n", [I]),
        
        {ok, Env} = elmdb:env_open(TestDir, []),
        {ok, DB} = elmdb:db_open(Env, [create]),
        ok = elmdb:put(DB, <<"rapid">>, <<"test">>),
        
        % Use env_close_by_name instead of env_close
        ok = elmdb:env_close_by_name(TestDir),
        
        % Immediately try to reopen (no delay)
        case elmdb:env_open(TestDir, []) of
            {ok, Env2} ->
                case elmdb:db_open(Env2, []) of
                    {ok, _DB2} ->
                        ok = elmdb:env_close(Env2),
                        io:format("    Cycle ~p: SUCCESS~n", [I]);
                    {error, DbError} ->
                        io:format("    Cycle ~p: DB OPEN FAILED: ~p~n", [I, DbError]),
                        ok = elmdb:env_close(Env2),
                        throw({db_open_failed_cycle, I, DbError})
                end;
            {error, already_open} ->
                io:format("    Cycle ~p: ENV STILL MARKED OPEN~n", [I]),
                throw({env_still_open_cycle, I});
            {error, EnvError} ->
                io:format("    Cycle ~p: ENV OPEN FAILED: ~p~n", [I, EnvError]),
                throw({env_open_failed_cycle, I, EnvError})
        end
    end, lists:seq(1, 20)),
    
    io:format("Aggressive race test completed successfully~n"),
    cleanup_dir(TestDir),
    success.

%% Helper function for directory cleanup
cleanup_dir(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok;
        _ -> ok
    end.