#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/elmdb/ebin

-mode(compile).

main(_) ->
    % Clean up and create test directory
    TestDir = "/tmp/elmdb_list_perf",
    os:cmd("rm -rf " ++ TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    % Open environment with performance settings
    {ok, Env} = elmdb:env_open(TestDir, [
        {map_size, 1024*1024*1024},
        no_sync,
        no_mem_init,
        write_map
    ]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    io:format("~nElmDB List Performance Test~n"),
    io:format("============================~n~n"),
    
    % Test 1: Hierarchical data
    io:format("Setting up hierarchical test data...~n"),
    setup_hierarchical_data(DB),
    
    io:format("~nTesting list performance:~n"),
    
    % Warm up
    elmdb:list(DB, <<"users/">>),
    
    % Test different prefixes
    test_list_performance(DB, <<"/">>),
    test_list_performance(DB, <<"users/">>),
    test_list_performance(DB, <<"users/user100/">>),
    test_list_performance(DB, <<"docs/">>),
    test_list_performance(DB, <<"config/">>),
    
    % Test 2: Sparse data
    io:format("~nSetting up sparse test data...~n"),
    setup_sparse_data(DB),
    
    io:format("~nTesting sparse data performance:~n"),
    test_list_performance(DB, <<"sparse/group1/">>),
    test_list_performance(DB, <<"sparse/group500/">>),
    test_list_performance(DB, <<"sparse/group999/">>),
    
    elmdb:env_close(Env),
    io:format("~nTest completed.~n").

setup_hierarchical_data(DB) ->
    % Create hierarchical structure like a filesystem
    % /users/userN/profile, settings, data
    % /docs/docN/content, metadata
    % /config/section/key
    
    lists:foreach(fun(I) ->
        UserNum = integer_to_binary(I),
        elmdb:put(DB, <<"users/user", UserNum/binary, "/profile">>, <<"profile_data">>),
        elmdb:put(DB, <<"users/user", UserNum/binary, "/settings">>, <<"settings_data">>),
        elmdb:put(DB, <<"users/user", UserNum/binary, "/data">>, <<"user_data">>)
    end, lists:seq(1, 1000)),
    
    lists:foreach(fun(I) ->
        DocNum = integer_to_binary(I),
        elmdb:put(DB, <<"docs/doc", DocNum/binary, "/content">>, <<"content_data">>),
        elmdb:put(DB, <<"docs/doc", DocNum/binary, "/metadata">>, <<"meta_data">>)
    end, lists:seq(1, 500)),
    
    lists:foreach(fun(I) ->
        ConfigNum = integer_to_binary(I),
        elmdb:put(DB, <<"config/section", ConfigNum/binary, "/key1">>, <<"value1">>),
        elmdb:put(DB, <<"config/section", ConfigNum/binary, "/key2">>, <<"value2">>)
    end, lists:seq(1, 100)),
    
    io:format("Created ~p hierarchical entries~n", [1000*3 + 500*2 + 100*2]).

setup_sparse_data(DB) ->
    % Create sparse groups spread across keyspace
    lists:foreach(fun(I) ->
        GroupNum = integer_to_binary(I),
        % Only create a few items per group
        elmdb:put(DB, <<"sparse/group", GroupNum/binary, "/item1">>, <<"data1">>),
        elmdb:put(DB, <<"sparse/group", GroupNum/binary, "/item2">>, <<"data2">>)
    end, lists:seq(1, 1000)),
    
    io:format("Created ~p sparse entries~n", [1000*2]).

test_list_performance(DB, Prefix) ->
    % Run multiple iterations and average
    Times = lists:map(fun(_) ->
        {Time, Result} = timer:tc(fun() -> elmdb:list(DB, Prefix) end),
        case Result of
            {ok, Children} ->
                {Time, length(Children)};
            not_found ->
                {Time, 0}
        end
    end, lists:seq(1, 100)),
    
    {TotalTime, TotalChildren} = lists:foldl(fun({T, C}, {AccT, AccC}) ->
        {AccT + T, AccC + C}
    end, {0, 0}, Times),
    
    AvgTime = TotalTime / 100,
    ChildCount = TotalChildren div 100,
    
    io:format("  ~-25s: ~8.2f μs, ~4p children~n", 
              [binary_to_list(Prefix), AvgTime, ChildCount]).