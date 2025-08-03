#!/usr/bin/env escript

main(_) ->
    % Add the path to the ebin directory
    code:add_path("_build/default/lib/elmdb/ebin"),
    
    % Try to load the module
    case code:load_file(elmdb) of
        {module, elmdb} ->
            io:format("Module loaded successfully~n"),
            
            % Check if functions are exported
            Exports = elmdb:module_info(exports),
            io:format("Exports: ~p~n", [Exports]),
            
            % Test NIF initialization
            case catch elmdb:init() of
                ok ->
                    io:format("NIF initialized successfully~n"),
                    test_operations();
                {error, {reload, _}} ->
                    io:format("NIF already loaded~n"),
                    test_operations();
                Error ->
                    io:format("NIF init error: ~p~n", [Error])
            end;
        {error, Reason} ->
            io:format("Failed to load module: ~p~n", [Reason])
    end.

test_operations() ->
    % Test basic operations
    TestDir = "/tmp/test_elmdb_direct",
    os:cmd("rm -rf " ++ TestDir ++ " && mkdir -p " ++ TestDir),
    
    io:format("Testing environment open...~n"),
    case catch elmdb:env_open(TestDir, []) of
        {ok, Env} ->
            io:format("Environment opened: ~p~n", [Env]),
            test_database(Env);
        {error, Reason} ->
            io:format("Failed to open environment: ~p~n", [Reason]);
        Error ->
            io:format("Environment open crashed: ~p~n", [Error])
    end.

test_database(Env) ->
    io:format("Testing database open...~n"),
    case catch elmdb:db_open(Env, [create]) of
        {ok, DB} ->
            io:format("Database opened: ~p~n", [DB]),
            test_put_get(DB, Env);
        {error, Reason} ->
            io:format("Failed to open database: ~p~n", [Reason]);
        Error ->
            io:format("Database open crashed: ~p~n", [Error])
    end.

test_put_get(DB, Env) ->
    io:format("Testing put operation...~n"),
    case catch elmdb:put(DB, <<"test_key">>, <<"test_value">>) of
        ok ->
            io:format("Put successful~n"),
            
            io:format("Testing get operation...~n"),
            case catch elmdb:get(DB, <<"test_key">>) of
                {ok, Value} ->
                    io:format("Get successful: ~p~n", [Value]);
                not_found ->
                    io:format("Key not found~n");
                {error, Reason} ->
                    io:format("Get failed: ~p~n", [Reason]);
                Error ->
                    io:format("Get crashed: ~p~n", [Error])
            end;
        {error, Reason} ->
            io:format("Put failed: ~p~n", [Reason]);
        Error ->
            io:format("Put crashed: ~p~n", [Error])
    end,
    
    % Clean up
    catch elmdb:env_close(Env).