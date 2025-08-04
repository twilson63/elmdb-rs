-module(test_reset).
-export([test/0]).

test() ->
    Path = "test_db_reset",
    
    %% Make sure the directory exists
    file:make_dir(Path),
    
    %% Open environment and database
    io:format("Opening environment...~n"),
    {ok, Env} = elmdb:env_open(Path, [{map_size, 1024 * 1024 * 10}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    %% Put some test data
    io:format("Writing test data...~n"),
    ok = elmdb:put(DB, <<"key1">>, <<"value1">>),
    ok = elmdb:put(DB, <<"key2">>, <<"value2">>),
    ok = elmdb:put(DB, <<"key3">>, <<"value3">>),
    
    %% Verify data exists
    io:format("Verifying data...~n"),
    {ok, <<"value1">>} = elmdb:get(DB, <<"key1">>),
    {ok, <<"value2">>} = elmdb:get(DB, <<"key2">>),
    {ok, <<"value3">>} = elmdb:get(DB, <<"key3">>),
    
    %% Close database and environment
    io:format("Closing database and environment...~n"),
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    
    %% Reset the database
    io:format("Resetting database...~n"),
    {ok, NewEnv, NewDB} = elmdb:reset(Path, [{map_size, 1024 * 1024 * 10}]),
    
    %% Verify data is gone
    io:format("Verifying data is gone after reset...~n"),
    not_found = elmdb:get(NewDB, <<"key1">>),
    not_found = elmdb:get(NewDB, <<"key2">>),
    not_found = elmdb:get(NewDB, <<"key3">>),
    
    %% Write new data to verify database is working
    io:format("Writing new data...~n"),
    ok = elmdb:put(NewDB, <<"newkey1">>, <<"newvalue1">>),
    {ok, <<"newvalue1">>} = elmdb:get(NewDB, <<"newkey1">>),
    
    %% Clean up
    io:format("Cleaning up...~n"),
    ok = elmdb:db_close(NewDB),
    ok = elmdb:env_close(NewEnv),
    
    %% Remove test directory
    os:cmd("rm -rf " ++ Path),
    
    io:format("Test passed!~n"),
    ok.