#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc
%%% ElmDB Regression Test Suite
%%% 
%%% Ensures all existing functions continue to work correctly
%%% after adding the new match function.
%%% @end
%%%-------------------------------------------------------------------

main(_Args) ->
    io:format("~n=== ElmDB Regression Test Suite ===~n~n"),
    
    case code:ensure_loaded(elmdb) of
        {module, elmdb} ->
            run_regression_tests();
        {error, Reason} ->
            io:format("ERROR: Failed to load elmdb module: ~p~n", [Reason]),
            halt(1)
    end.

run_regression_tests() ->
    TestDir = "/tmp/elmdb_regression_" ++ integer_to_list(erlang:system_time()),
    ok = filelib:ensure_dir(filename:join(TestDir, "dummy")),
    
    io:format("Test directory: ~s~n~n", [TestDir]),
    
    Tests = [
        {"Environment Operations", fun() -> test_env_operations(TestDir) end},
        {"Basic CRUD Operations", fun() -> test_crud_operations(TestDir) end},
        {"Batch Operations", fun() -> test_batch_operations(TestDir) end},
        {"List Operations", fun() -> test_list_operations(TestDir) end},
        {"Flush Operations", fun() -> test_flush_operations(TestDir) end},
        {"Error Handling", fun() -> test_error_handling(TestDir) end},
        {"Large Data Handling", fun() -> test_large_data(TestDir) end}
    ],
    
    Results = lists:map(fun({Name, Test}) ->
        io:format("~n--- Testing: ~s ---~n", [Name]),
        try
            StartTime = erlang:monotonic_time(millisecond),
            Test(),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,
            io:format("✓ Passed (~p ms)~n", [Duration]),
            {Name, pass, Duration}
        catch
            Type:Error:Stack ->
                io:format("✗ FAILED: ~p:~p~n", [Type, Error]),
                io:format("  Stack: ~p~n", [Stack]),
                {Name, fail, {Type, Error}}
        end
    end, Tests),
    
    % Print summary
    io:format("~n=== REGRESSION TEST SUMMARY ===~n"),
    Passed = length([ok || {_, pass, _} <- Results]),
    Failed = length([ok || {_, fail, _} <- Results]),
    io:format("Passed: ~p/~p~n", [Passed, length(Results)]),
    io:format("Failed: ~p/~p~n", [Failed, length(Results)]),
    
    lists:foreach(fun
        ({Name, fail, _}) -> io:format("  ✗ ~s~n", [Name]);
        (_) -> ok
    end, Results),
    
    % Cleanup
    file:del_dir_r(TestDir),
    
    if Failed > 0 -> halt(1); true -> halt(0) end.

%% Test environment operations
test_env_operations(BaseDir) ->
    Dir1 = filename:join(BaseDir, "env1"),
    Dir2 = filename:join(BaseDir, "env2"),
    ok = filelib:ensure_dir(filename:join(Dir1, "dummy")),
    ok = filelib:ensure_dir(filename:join(Dir2, "dummy")),
    
    % Test env_open with various options
    {ok, Env1} = elmdb:env_open(Dir1, [{map_size, 10485760}]),
    {ok, Env2} = elmdb:env_open(Dir2, [{map_size, 20971520}, no_sync]),
    
    % Test env_status
    {ok, false, _, _} = elmdb:env_status(Env1),
    
    % Test env_close
    ok = elmdb:env_close(Env1),
    ok = elmdb:env_close(Env2),
    
    % Test env_close_by_name
    {ok, Env3} = elmdb:env_open(Dir1, [{map_size, 10485760}]),
    ok = elmdb:env_close_by_name(Dir1),
    
    io:format("  ✓ Environment operations work correctly~n").

%% Test basic CRUD operations
test_crud_operations(BaseDir) ->
    Dir = filename:join(BaseDir, "crud"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test put and get
    ok = elmdb:put(DB, <<"key1">>, <<"value1">>),
    {ok, <<"value1">>} = elmdb:get(DB, <<"key1">>),
    
    % Test overwrite
    ok = elmdb:put(DB, <<"key1">>, <<"value2">>),
    {ok, <<"value2">>} = elmdb:get(DB, <<"key1">>),
    
    % Test not found
    not_found = elmdb:get(DB, <<"nonexistent">>),
    
    % Test binary keys and values
    BinaryKey = <<1,2,3,4,5>>,
    BinaryValue = <<255,254,253,252,251>>,
    ok = elmdb:put(DB, BinaryKey, BinaryValue),
    {ok, BinaryValue} = elmdb:get(DB, BinaryKey),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  ✓ CRUD operations work correctly~n").

%% Test batch operations
test_batch_operations(BaseDir) ->
    Dir = filename:join(BaseDir, "batch"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test put_batch
    Batch = [
        {<<"batch_key1">>, <<"batch_value1">>},
        {<<"batch_key2">>, <<"batch_value2">>},
        {<<"batch_key3">>, <<"batch_value3">>}
    ],
    ok = elmdb:put_batch(DB, Batch),
    
    % Verify batch write
    {ok, <<"batch_value1">>} = elmdb:get(DB, <<"batch_key1">>),
    {ok, <<"batch_value2">>} = elmdb:get(DB, <<"batch_key2">>),
    {ok, <<"batch_value3">>} = elmdb:get(DB, <<"batch_key3">>),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  ✓ Batch operations work correctly~n").

%% Test list operations
test_list_operations(BaseDir) ->
    Dir = filename:join(BaseDir, "list"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Create hierarchical data
    ok = elmdb:put(DB, <<"users">>, <<>>),
    ok = elmdb:put(DB, <<"users/alice">>, <<>>),
    ok = elmdb:put(DB, <<"users/bob">>, <<>>),
    ok = elmdb:put(DB, <<"users/charlie">>, <<>>),
    ok = elmdb:put(DB, <<"groups">>, <<>>),
    ok = elmdb:put(DB, <<"groups/admin">>, <<>>),
    ok = elmdb:put(DB, <<"groups/users">>, <<>>),
    
    % Test list
    {ok, Children} = elmdb:list(DB, <<"users">>),
    true = lists:member(<<"users/alice">>, Children),
    true = lists:member(<<"users/bob">>, Children),
    true = lists:member(<<"users/charlie">>, Children),
    
    % Test list with non-existent prefix
    not_found = elmdb:list(DB, <<"nonexistent">>),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  ✓ List operations work correctly~n").

%% Test flush operations
test_flush_operations(BaseDir) ->
    Dir = filename:join(BaseDir, "flush"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Write some data
    lists:foreach(fun(I) ->
        Key = iolist_to_binary([<<"flush_key_">>, integer_to_binary(I)]),
        Value = iolist_to_binary([<<"flush_value_">>, integer_to_binary(I)]),
        ok = elmdb:put(DB, Key, Value)
    end, lists:seq(1, 100)),
    
    % Test explicit flush
    ok = elmdb:flush(DB),
    
    % Verify data persisted
    {ok, <<"flush_value_50">>} = elmdb:get(DB, <<"flush_key_50">>),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  ✓ Flush operations work correctly~n").

%% Test error handling
test_error_handling(BaseDir) ->
    Dir = filename:join(BaseDir, "errors"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    % Test opening non-existent directory without create flag
    NonExistentDir = filename:join(BaseDir, "nonexistent"),
    {error, directory_not_found} = elmdb:env_open(NonExistentDir, []),
    
    % Test double open
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}]),
    {error, already_open} = elmdb:env_open(Dir, [{map_size, 10485760}]),
    
    % Test operations on closed database
    {ok, DB} = elmdb:db_open(Env, [create]),
    ok = elmdb:db_close(DB),
    {error, database_error, _} = elmdb:put(DB, <<"key">>, <<"value">>),
    
    elmdb:env_close(Env),
    io:format("  ✓ Error handling works correctly~n").

%% Test large data handling
test_large_data(BaseDir) ->
    Dir = filename:join(BaseDir, "large"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 104857600}]), % 100MB
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Test large value
    LargeValue = binary:copy(<<"X">>, 100000), % 100KB
    ok = elmdb:put(DB, <<"large_key">>, LargeValue),
    {ok, Retrieved} = elmdb:get(DB, <<"large_key">>),
    true = (byte_size(Retrieved) =:= 100000),
    
    % Test many keys
    lists:foreach(fun(I) ->
        Key = iolist_to_binary([<<"many_">>, integer_to_binary(I)]),
        Value = iolist_to_binary([<<"value_">>, integer_to_binary(I)]),
        ok = elmdb:put(DB, Key, Value)
    end, lists:seq(1, 10000)),
    
    % Verify random access
    {ok, <<"value_5000">>} = elmdb:get(DB, <<"many_5000">>),
    {ok, <<"value_9999">>} = elmdb:get(DB, <<"many_9999">>),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  ✓ Large data handling works correctly~n").