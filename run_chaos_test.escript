#!/usr/bin/env escript

main(_) ->
    % Add paths
    code:add_path("_build/default/lib/elmdb/ebin"),
    code:add_path("test"),
    
    % Load elmdb module
    case code:ensure_loaded(elmdb) of
        {module, elmdb} ->
            io:format("ElmDB module loaded successfully~n");
        Error ->
            io:format("Failed to load elmdb: ~p~n", [Error]),
            halt(1)
    end,
    
    % Compile and load the chaos test
    case compile:file("test/elmdb_chaos_test.erl", [{outdir, "test"}]) of
        {ok, _} ->
            io:format("Chaos test compiled successfully~n");
        CompileError ->
            io:format("Failed to compile chaos test: ~p~n", [CompileError]),
            halt(1)
    end,
    
    code:load_file(elmdb_chaos_test),
    
    % Run the chaos test
    io:format("~n=== Starting ElmDB Chaos Monkey Test ===~n"),
    io:format("Target database size: 100 GB~n"),
    io:format("This will take up to 2 hours to complete...~n~n"),
    
    Result = elmdb_chaos_test:run_chaos_test(),
    
    io:format("~nTest completed with result: ~p~n", [Result]),
    
    case Result of
        ok -> halt(0);
        _ -> halt(1)
    end.