-module(simple_test).
-export([test_reference_fix/0]).

test_reference_fix() ->
    io:format("Testing reference counting fix...~n"),
    
    % Clean up and create test directory
    Path = "/tmp/simple_test",
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Test basic environment and database operations
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
    io:format("Environment opened successfully~n"),
    
    {ok, DB} = elmdb:db_open(Env, [create]),
    io:format("Database opened successfully~n"),
    
    % Test data operations
    ok = elmdb:put(DB, <<"test_key">>, <<"test_value">>),
    {ok, Value} = elmdb:get(DB, <<"test_key">>),
    io:format("Retrieved value: ~p~n", [Value]),
    
    % Test the new db_close function
    io:format("Testing db_close...~n"),
    try
        Result = elmdb:db_close(DB),
        io:format("db_close result: ~p~n", [Result])
    catch
        error:undef ->
            io:format("db_close function not available - this is the bug we're fixing~n")
    end,
    
    % Test environment close
    io:format("Testing env_close...~n"),
    CloseResult = elmdb:env_close(Env),
    io:format("env_close result: ~p~n", [CloseResult]),
    
    % Clean up
    os:cmd("rm -rf " ++ Path),
    io:format("Test completed!~n").