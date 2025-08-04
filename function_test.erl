-module(function_test).
-export([test_all_functions/0]).

test_all_functions() ->
    io:format("Testing all available functions...~n"),
    
    Path = "/tmp/function_test",
    os:cmd("rm -rf " ++ Path),
    os:cmd("mkdir -p " ++ Path),
    
    % Test basic functions that should work
    io:format("Testing env_open...~n"),
    try
        {ok, Env} = elmdb:env_open(Path, [{map_size, 1024*1024*10}]),
        io:format("  env_open: SUCCESS~n"),
        
        io:format("Testing db_open...~n"),
        {ok, DB} = elmdb:db_open(Env, [create]),
        io:format("  db_open: SUCCESS~n"),
        
        io:format("Testing put...~n"),
        ok = elmdb:put(DB, <<"test">>, <<"value">>),
        io:format("  put: SUCCESS~n"),
        
        io:format("Testing get...~n"),
        {ok, Value} = elmdb:get(DB, <<"test">>),
        io:format("  get: SUCCESS (value: ~p)~n", [Value]),
        
        io:format("Testing flush...~n"),
        ok = elmdb:flush(DB),
        io:format("  flush: SUCCESS~n"),
        
        % Test new functions
        io:format("Testing db_close...~n"),
        try
            DbCloseResult = elmdb:db_close(DB),
            io:format("  db_close: SUCCESS (~p)~n", [DbCloseResult])
        catch
            error:undef ->
                io:format("  db_close: NOT AVAILABLE (undef)~n");
            error:nif_not_loaded ->
                io:format("  db_close: NOT LOADED (nif_not_loaded)~n")
        end,
        
        io:format("Testing env_force_close...~n"),
        try
            ForceCloseResult = elmdb:env_force_close(Env),
            io:format("  env_force_close: SUCCESS (~p)~n", [ForceCloseResult])
        catch
            error:undef ->
                io:format("  env_force_close: NOT AVAILABLE (undef)~n");
            error:nif_not_loaded ->
                io:format("  env_force_close: NOT LOADED (nif_not_loaded)~n")
        end,
        
        io:format("Testing regular env_close...~n"),
        try
            EnvCloseResult = elmdb:env_close(Env),
            io:format("  env_close: SUCCESS (~p)~n", [EnvCloseResult])
        catch
            error:undef ->
                io:format("  env_close: NOT AVAILABLE (undef)~n");
            error:nif_not_loaded ->
                io:format("  env_close: NOT LOADED (nif_not_loaded)~n")
        end
        
    catch
        Error:Reason ->
            io:format("ERROR: ~p:~p~n", [Error, Reason])
    end,
    
    os:cmd("rm -rf " ++ Path),
    ok.