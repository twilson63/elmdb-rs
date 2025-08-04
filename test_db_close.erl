#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/elmdb/ebin

-mode(compile).

main(_) ->
    TestDir = "/tmp/elmdb_close_test",
    os:cmd("rm -rf " ++ TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    io:format("Testing database close functionality~n"),
    io:format("====================================~n~n"),
    
    % Test 1: Normal close with explicit db_close
    io:format("Test 1: Explicit db_close before env_close~n"),
    {ok, Env1} = elmdb:env_open(TestDir ++ "/test1", [{map_size, 10485760}]),
    {ok, DB1} = elmdb:db_open(Env1, [create]),
    ok = elmdb:put(DB1, <<"key1">>, <<"value1">>),
    {ok, <<"value1">>} = elmdb:get(DB1, <<"key1">>),
    
    % Explicitly close database first
    ok = elmdb:db_close(DB1),
    io:format("  Database closed successfully~n"),
    
    % Now environment should close without error
    ok = elmdb:env_close(Env1),
    io:format("  Environment closed successfully~n"),
    
    % Test 2: Force close with active database
    io:format("~nTest 2: Force close with active database~n"),
    {ok, Env2} = elmdb:env_open(TestDir ++ "/test2", [{map_size, 10485760}]),
    {ok, DB2} = elmdb:db_open(Env2, [create]),
    ok = elmdb:put(DB2, <<"key2">>, <<"value2">>),
    
    % Force close without closing database
    case elmdb:env_force_close(Env2) of
        {ok, Warning} ->
            io:format("  Force close succeeded with warning: ~s~n", [Warning]);
        ok ->
            io:format("  Force close succeeded~n")
    end,
    
    % Test 3: Multiple databases on same environment
    io:format("~nTest 3: Multiple databases, proper cleanup~n"),
    {ok, Env3} = elmdb:env_open(TestDir ++ "/test3", [{map_size, 10485760}]),
    {ok, DB3a} = elmdb:db_open(Env3, [create]),
    {ok, DB3b} = elmdb:db_open(Env3, []),  % Open second handle to same DB
    
    ok = elmdb:put(DB3a, <<"key3">>, <<"value3">>),
    {ok, <<"value3">>} = elmdb:get(DB3b, <<"key3">>),
    
    % Close both database handles
    ok = elmdb:db_close(DB3a),
    io:format("  First database handle closed~n"),
    ok = elmdb:db_close(DB3b),
    io:format("  Second database handle closed~n"),
    
    % Now environment should close
    ok = elmdb:env_close(Env3),
    io:format("  Environment closed successfully~n"),
    
    % Test 4: Try to close environment with active database (should fail)
    io:format("~nTest 4: Try normal close with active database~n"),
    {ok, Env4} = elmdb:env_open(TestDir ++ "/test4", [{map_size, 10485760}]),
    {ok, DB4} = elmdb:db_open(Env4, [create]),
    ok = elmdb:put(DB4, <<"key4">>, <<"value4">>),
    
    case elmdb:env_close(Env4) of
        {error, _, Msg} ->
            io:format("  Expected error: ~s~n", [Msg]);
        ok ->
            io:format("  ERROR: Should have failed!~n")
    end,
    
    % Clean up with force close
    elmdb:env_force_close(Env4),
    
    % Test 5: Force close by name
    io:format("~nTest 5: Force close by name~n"),
    TestPath5 = TestDir ++ "/test5",
    {ok, Env5} = elmdb:env_open(TestPath5, [{map_size, 10485760}]),
    {ok, DB5} = elmdb:db_open(Env5, [create]),
    ok = elmdb:put(DB5, <<"key5">>, <<"value5">>),
    
    % Force close by path name
    case elmdb:env_force_close_by_name(TestPath5) of
        {ok, Warning2} ->
            io:format("  Force close by name succeeded with warning: ~s~n", [Warning2]);
        ok ->
            io:format("  Force close by name succeeded~n")
    end,
    
    io:format("~nAll tests completed successfully!~n").