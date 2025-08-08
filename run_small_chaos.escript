#!/usr/bin/env escript

main(_) ->
    % Add paths
    code:add_path("_build/default/lib/elmdb/ebin"),
    code:add_path("test"),
    
    % Load elmdb module
    case code:ensure_loaded(elmdb) of
        {module, elmdb} ->
            io:format("ElmDB module loaded successfully~n");
        LoadError ->
            io:format("Failed to load elmdb: ~p~n", [LoadError]),
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
    
    % Run a smaller 10GB test first
    io:format("~n=== Starting Small ElmDB Chaos Test (10GB) ===~n"),
    
    GB = 1024 * 1024 * 1024,
    State = elmdb_chaos_test:init_chaos_test("/tmp/elmdb_chaos_10gb", 10 * GB),
    
    try
        io:format("Running chaos operations for 60 seconds...~n"),
        State2 = elmdb_chaos_test:run_chaos_operations(State, 60000),
        
        io:format("~nCleaning up...~n"),
        elmdb_chaos_test:cleanup_chaos_test(State2),
        
        io:format("~nGenerating report...~n"),
        elmdb_chaos_test:report_chaos_results(State2),
        
        io:format("~nTest completed successfully!~n"),
        halt(0)
    catch
        Type:Err:Stack ->
            io:format("Test failed: ~p:~p~n~p~n", [Type, Err, Stack]),
            elmdb_chaos_test:cleanup_chaos_test(State),
            halt(1)
    end.