#!/usr/bin/env escript

main(_) ->
    io:format("Starting cursor debug test...~n"),
    
    % Clean up any previous test data
    TestPath = "/tmp/elmdb_cursor_debug",
    os:cmd("rm -rf " ++ TestPath),
    
    % Load the elmdb module
    code:add_path("./ebin"),
    code:add_path("./_build/default/lib/elmdb/ebin"),
    c:l(elmdb),
    
    % Step 1: Open environment
    {ok, Env} = elmdb:env_open(TestPath, [{map_size, 1024*1024*100}]),
    io:format("Environment opened successfully~n"),
    
    % Step 2: Open database
    {ok, Db} = elmdb:db_open(Env, [create]),
    io:format("Database opened successfully~n"),
    
    % Step 3: Write some test data
    TestKeys = [
        {<<"test/key1">>, <<"value1">>},
        {<<"test/key2">>, <<"value2">>},
        {<<"other/key3">>, <<"value3">>},
        {<<"test/sub/key4">>, <<"value4">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(Db, Key, Value),
        io:format("Put ~p -> ~p~n", [Key, Value])
    end, TestKeys),
    
    % Step 4: Flush writes
    ok = elmdb:flush(Db),
    io:format("Flushed writes~n"),
    
    % Step 5: Test get operations (these should work)
    io:format("~n=== Testing get operations ===~n"),
    lists:foreach(fun({Key, ExpectedValue}) ->
        case elmdb:get(Db, Key) of
            {ok, Value} when Value =:= ExpectedValue ->
                io:format("GET OK: ~p -> ~p~n", [Key, Value]);
            {ok, Value} ->
                io:format("GET MISMATCH: ~p -> ~p (expected ~p)~n", [Key, Value, ExpectedValue]);
            Other ->
                io:format("GET FAILED: ~p -> ~p~n", [Key, Other])
        end
    end, TestKeys),
    
    % Step 6: Test debug database state
    io:format("~n=== Testing debug database state ===~n"),
    case elmdb:debug_db_state(Db) of
        {ok, Stats} ->
            io:format("Database stats: ~p~n", [Stats]);
        Error ->
            io:format("Failed to get database stats: ~p~n", [Error])
    end,
    
    % Step 7: Test count_keys function
    io:format("~n=== Testing count_keys ===~n"),
    case elmdb:count_keys(Db) of
        {ok, Count} ->
            io:format("Key count: ~p~n", [Count]);
        Error ->
            io:format("Failed to count keys: ~p~n", [Error])
    end,
    
    % Step 8: Test list operation (this is the problematic one)
    io:format("~n=== Testing list operation ===~n"),
    case elmdb:list(Db, <<"test">>) of
        {ok, Children} ->
            io:format("List success: ~p~n", [Children]);
        {error, not_found, DebugMsg} ->
            io:format("List returned not_found with debug: ~p~n", [DebugMsg]);
        Error ->
            io:format("List failed: ~p~n", [Error])
    end,
    
    % Cleanup
    ok = elmdb:db_close(Db),
    ok = elmdb:env_close(Env),
    io:format("~nTest completed~n").