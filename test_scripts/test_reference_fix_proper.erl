-module(test_reference_fix_proper).
-export([run_tests/0]).

run_tests() ->
    io:format("Testing reference counting fix...~n"),
    
    % Test 1: Normal close with explicit db_close
    test_explicit_db_close(),
    
    % Test 2: Force close with active references
    test_force_close(),
    
    % Test 3: Close by name with proper cleanup
    test_close_by_name(),
    
    io:format("All tests completed!~n").

test_explicit_db_close() ->
    io:format("Test 1: Explicit database close...~n"),
    Path = "/tmp/test_ref_fix_1",
    
    % Clean up any existing database and create directory
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Open environment
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
    
    % Open database
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Put some data
    ok = elmdb:put(DB, <<"key1">>, <<"value1">>),
    
    % Check status before close
    {ok, false, RefCount1, _} = elmdb:env_status(Env),
    io:format("  Reference count before db_close: ~p~n", [RefCount1]),
    
    % Explicitly close database
    ok = elmdb:db_close(DB),
    
    % Check status after db close
    {ok, false, RefCount2, _} = elmdb:env_status(Env),
    io:format("  Reference count after db_close: ~p~n", [RefCount2]),
    
    % Now environment close should work
    ok = elmdb:env_close(Env),
    io:format("  Environment closed successfully!~n"),
    
    % Clean up
    os:cmd("rm -rf " ++ Path),
    ok.

test_force_close() ->
    io:format("Test 2: Force close with active references...~n"),
    Path = "/tmp/test_ref_fix_2", 
    
    % Clean up any existing database and create directory
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Open environment
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
    
    % Open database but don't close it
    {ok, _DB} = elmdb:db_open(Env, [create]),
    
    % Check status - should have 1 reference
    {ok, false, RefCount, _} = elmdb:env_status(Env),
    io:format("  Reference count with active DB: ~p~n", [RefCount]),
    
    % Normal close should fail
    Result = elmdb:env_close(Env),
    io:format("  Normal close result: ~p~n", [Result]),
    
    % Force close should work
    ForceResult = elmdb:env_force_close(Env),
    io:format("  Force close result: ~p~n", [ForceResult]),
    
    % Clean up
    os:cmd("rm -rf " ++ Path),
    ok.

test_close_by_name() ->
    io:format("Test 3: Close by name with proper cleanup...~n"),
    Path = "/tmp/test_ref_fix_3",
    
    % Clean up any existing database and create directory
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Open environment
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
    
    % Open and close database properly
    {ok, DB} = elmdb:db_open(Env, [create]),
    ok = elmdb:put(DB, <<"key1">>, <<"value1">>),
    ok = elmdb:db_close(DB),
    
    % Check status - should have 0 references
    {ok, false, RefCount, _} = elmdb:env_status(Env),
    io:format("  Reference count after proper cleanup: ~p~n", [RefCount]),
    
    % Close by name should work
    ok = elmdb:env_close_by_name(Path),
    io:format("  Environment closed by name successfully!~n"),
    
    % Clean up
    os:cmd("rm -rf " ++ Path),
    ok.