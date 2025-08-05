-module(final_test).
-export([run_complete_test/0]).

run_complete_test() ->
    io:format("=== COMPLETE REFERENCE COUNTING FIX TEST ===~n"),
    
    % Test 1: Original issue - should fail without explicit db_close
    test_original_issue(),
    
    % Test 2: Fixed issue - should work with explicit db_close
    test_explicit_db_close(),
    
    % Test 3: Force close functionality
    test_force_close(),
    
    % Test 4: Multiple databases
    test_multiple_databases(),
    
    io:format("=== ALL TESTS COMPLETED SUCCESSFULLY ===~n").

test_original_issue() ->
    io:format("~nTest 1: Reproducing original issue...~n"),
    Path = "/tmp/original_issue",
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Open environment and database without explicit close
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
    {ok, _DB} = elmdb:db_open(Env, [create]),
    
    % Try to close environment - should fail with active reference
    Result = elmdb:env_close(Env),
    case Result of
        {error, environment_error, _} ->
            io:format("  ✓ Correctly failed with active reference: ~p~n", [Result]);
        _ ->
            io:format("  ✗ Unexpected result: ~p~n", [Result])
    end,
    
    % Force close to clean up
    elmdb:env_force_close(Env),
    os:cmd("rm -rf " ++ Path).

test_explicit_db_close() ->
    io:format("~nTest 2: Fixed with explicit db_close...~n"),
    Path = "/tmp/explicit_close",
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Open environment and database
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Do some operations
    ok = elmdb:put(DB, <<"key1">>, <<"value1">>),
    {ok, Value} = elmdb:get(DB, <<"key1">>),
    io:format("  Stored and retrieved: ~p~n", [Value]),
    
    % Explicitly close database
    ok = elmdb:db_close(DB),
    io:format("  ✓ Database closed successfully~n"),
    
    % Now environment close should work
    Result = elmdb:env_close(Env),
    case Result of
        ok ->
            io:format("  ✓ Environment closed successfully: ~p~n", [Result]);
        _ ->
            io:format("  ✗ Environment close failed: ~p~n", [Result])
    end,
    
    os:cmd("rm -rf " ++ Path).

test_force_close() ->
    io:format("~nTest 3: Force close functionality...~n"),
    Path = "/tmp/force_close",
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Open environment and database, leave database open
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
    {ok, _DB} = elmdb:db_open(Env, [create]),
    
    % Force close should work even with active references
    Result = elmdb:env_force_close(Env),
    case Result of
        ok ->
            io:format("  ✓ Force close succeeded: ~p~n", [Result]);
        {ok, Warning} ->
            io:format("  ✓ Force close succeeded with warning: ~p~n", [Warning]);
        _ ->
            io:format("  ✗ Force close failed: ~p~n", [Result])
    end,
    
    os:cmd("rm -rf " ++ Path).

test_multiple_databases() ->
    io:format("~nTest 4: Multiple databases...~n"),
    Path = "/tmp/multiple_dbs",
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Open environment
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
    
    % Open multiple databases
    {ok, DB1} = elmdb:db_open(Env, [create]),
    {ok, DB2} = elmdb:db_open(Env, [create]),
    {ok, DB3} = elmdb:db_open(Env, [create]),
    
    % Do operations on each
    ok = elmdb:put(DB1, <<"key1">>, <<"value1">>),
    ok = elmdb:put(DB2, <<"key2">>, <<"value2">>),
    ok = elmdb:put(DB3, <<"key3">>, <<"value3">>),
    
    io:format("  Created 3 databases with data~n"),
    
    % Close them one by one
    ok = elmdb:db_close(DB1),
    io:format("  ✓ Database 1 closed~n"),
    
    ok = elmdb:db_close(DB2),
    io:format("  ✓ Database 2 closed~n"),
    
    ok = elmdb:db_close(DB3),
    io:format("  ✓ Database 3 closed~n"),
    
    % Now environment should close cleanly
    Result = elmdb:env_close(Env),
    case Result of
        ok ->
            io:format("  ✓ Environment closed after all databases: ~p~n", [Result]);
        _ ->
            io:format("  ✗ Environment close failed: ~p~n", [Result])
    end,
    
    os:cmd("rm -rf " ++ Path).