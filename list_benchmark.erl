%%%-------------------------------------------------------------------
%%% @doc
%%% ElmDB List Operation Benchmark
%%% 
%%% Specialized benchmark to measure list operation performance
%%% with different data patterns and prefix lengths.
%%% @end
%%%-------------------------------------------------------------------
-module(list_benchmark).

%% CT exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    bench_list_shallow/1,
    bench_list_deep/1,
    bench_list_sparse/1,
    bench_list_dense/1,
    bench_list_scaling/1
]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        bench_list_shallow,
        bench_list_deep,
        bench_list_sparse, 
        bench_list_dense,
        bench_list_scaling
    ].

init_per_suite(Config) ->
    case code:ensure_loaded(elmdb) of
        {module, elmdb} ->
            ct:print("ElmDB module loaded successfully"),
            Config;
        {error, Reason} ->
            ct:fail("Failed to load elmdb module: ~p", [Reason])
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    TestDir = filename:join([?config(priv_dir, Config), atom_to_list(TestCase)]),
    ok = filelib:ensure_dir(filename:join(TestDir, "dummy")),
    
    case catch elmdb:env_open(TestDir, [{map_size, 2*1024*1024*1024}, no_sync, no_mem_init, write_map]) of
        {ok, Env} ->
            case catch elmdb:db_open(Env, [create]) of
                {ok, DB} ->
                    [{test_dir, TestDir}, {env, Env}, {db, DB} | Config];
                {error, Reason} ->
                    ct:fail("Database open failed: ~p", [Reason]);
                Error ->
                    ct:fail("Database open crashed: ~p", [Error])
            end;
        {error, Reason} ->
            ct:fail("Environment open failed: ~p", [Reason]);
        Error ->
            {skip, "NIF not loaded properly"}
    end.

end_per_testcase(_TestCase, Config) ->
    case ?config(env, Config) of
        undefined -> ok;
        Env -> catch elmdb:env_close(Env)
    end,
    case ?config(test_dir, Config) of
        undefined -> ok;
        TestDir -> file:del_dir_r(TestDir)
    end,
    ok.

%%%===================================================================
%%% Benchmark Tests
%%%===================================================================

%% @doc Benchmark list operation on shallow hierarchy (2-3 levels)
bench_list_shallow(Config) ->
    DB = ?config(db, Config),
    
    ct:print("=== Shallow Hierarchy List Benchmark ==="),
    
    % Create shallow hierarchical data: /users/N, /groups/N, /settings/N
    TestData = generate_shallow_data(1000),
    write_test_data(DB, TestData),
    
    % Test different prefix patterns
    Prefixes = [
        <<"/users">>,
        <<"/groups">>, 
        <<"/settings">>,
        <<"/users/100">>,
        <<"/groups/50">>
    ],
    
    ct:print("Testing ~p prefixes with shallow data", [length(Prefixes)]),
    
    Results = benchmark_list_operations(DB, Prefixes, 100), % 100 iterations per prefix
    
    % Analysis
    TotalOps = length(Prefixes) * 100,
    TotalTime = lists:sum([Time || {_, _, Time, _} <- Results]),
    AvgTime = TotalTime / TotalOps,
    OpsPerSecond = 1000000 / AvgTime, % Convert microseconds to ops/sec
    
    ct:print("Shallow Hierarchy Results:"),
    ct:print("  Total operations: ~p", [TotalOps]),
    ct:print("  Average time per operation: ~.2f μs", [AvgTime]),
    ct:print("  Operations per second: ~.2f", [OpsPerSecond]),
    
    print_detailed_results(Results).

%% @doc Benchmark list operation on deep hierarchy (5-8 levels)
bench_list_deep(Config) ->
    DB = ?config(db, Config),
    
    ct:print("=== Deep Hierarchy List Benchmark ==="),
    
    % Create deep hierarchical data: /org/dept/team/user/N/profile/field
    TestData = generate_deep_data(200), % Fewer records but deeper
    write_test_data(DB, TestData),
    
    Prefixes = [
        <<"/org">>,
        <<"/org/engineering">>,
        <<"/org/engineering/backend">>,
        <<"/org/engineering/backend/user1">>,
        <<"/org/engineering/backend/user1/profile">>
    ],
    
    ct:print("Testing ~p prefixes with deep data", [length(Prefixes)]),
    
    Results = benchmark_list_operations(DB, Prefixes, 100),
    
    TotalOps = length(Prefixes) * 100,
    TotalTime = lists:sum([Time || {_, _, Time, _} <- Results]),
    AvgTime = TotalTime / TotalOps,
    OpsPerSecond = 1000000 / AvgTime,
    
    ct:print("Deep Hierarchy Results:"),
    ct:print("  Total operations: ~p", [TotalOps]),
    ct:print("  Average time per operation: ~.2f μs", [AvgTime]),
    ct:print("  Operations per second: ~.2f", [OpsPerSecond]),
    
    print_detailed_results(Results).

%% @doc Benchmark list operation on sparse data (many prefixes, few children each)
bench_list_sparse(Config) ->
    DB = ?config(db, Config),
    
    ct:print("=== Sparse Data List Benchmark ==="),
    
    % Create sparse data: many different prefixes with 1-3 children each
    TestData = generate_sparse_data(500), % 500 different prefix groups
    write_test_data(DB, TestData),
    
    % Test random sparse prefixes
    Prefixes = [
        <<"/sparse_1">>,
        <<"/sparse_50">>,
        <<"/sparse_100">>,
        <<"/sparse_250">>,
        <<"/sparse_499">>,
        <<"/nonexistent">>
    ],
    
    ct:print("Testing ~p prefixes with sparse data", [length(Prefixes)]),
    
    Results = benchmark_list_operations(DB, Prefixes, 200), % More iterations for sparse data
    
    TotalOps = length(Prefixes) * 200,
    TotalTime = lists:sum([Time || {_, _, Time, _} <- Results]),
    AvgTime = TotalTime / TotalOps,
    OpsPerSecond = 1000000 / AvgTime,
    
    ct:print("Sparse Data Results:"),
    ct:print("  Total operations: ~p", [TotalOps]),
    ct:print("  Average time per operation: ~.2f μs", [AvgTime]),
    ct:print("  Operations per second: ~.2f", [OpsPerSecond]),
    
    print_detailed_results(Results).

%% @doc Benchmark list operation on dense data (few prefixes, many children each)
bench_list_dense(Config) ->
    DB = ?config(db, Config),
    
    ct:print("=== Dense Data List Benchmark ==="),
    
    % Create dense data: few prefixes with many children each
    TestData = generate_dense_data(50, 100), % 50 prefixes, 100 children each
    write_test_data(DB, TestData),
    
    Prefixes = [
        <<"/dense_1">>,
        <<"/dense_10">>,
        <<"/dense_25">>,
        <<"/dense_40">>,
        <<"/dense_49">>
    ],
    
    ct:print("Testing ~p prefixes with dense data", [length(Prefixes)]),
    
    Results = benchmark_list_operations(DB, Prefixes, 50), % Fewer iterations for dense data
    
    TotalOps = length(Prefixes) * 50,
    TotalTime = lists:sum([Time || {_, _, Time, _} <- Results]),
    AvgTime = TotalTime / TotalOps,
    OpsPerSecond = 1000000 / AvgTime,
    
    ct:print("Dense Data Results:"),
    ct:print("  Total operations: ~p", [TotalOps]),
    ct:print("  Average time per operation: ~.2f μs", [AvgTime]),
    ct:print("  Operations per second: ~.2f", [OpsPerSecond]),
    
    print_detailed_results(Results).

%% @doc Benchmark list operation scaling with different result set sizes
bench_list_scaling(Config) ->
    DB = ?config(db, Config),
    
    ct:print("=== List Operation Scaling Benchmark ==="),
    
    % Create data with predictable scaling: prefixes with 1, 10, 100, 1000 children
    Scales = [1, 10, 100, 1000],
    TestData = lists:flatten([
        generate_scaling_data(Scale, Scale) || Scale <- Scales
    ]),
    write_test_data(DB, TestData),
    
    % Test each scale
    ScaleResults = lists:map(fun(Scale) ->
        Prefix = list_to_binary("/scale_" ++ integer_to_list(Scale)),
        StartTime = erlang:monotonic_time(microsecond),
        
        Result = elmdb:list(DB, Prefix),
        
        EndTime = erlang:monotonic_time(microsecond),
        Time = EndTime - StartTime,
        
        ChildCount = case Result of
            {ok, Children} -> length(Children);
            _ -> 0
        end,
        
        ct:print("  Scale ~p: ~p children in ~p μs (~.2f μs per child)", 
                [Scale, ChildCount, Time, Time / max(1, ChildCount)]),
        
        {Scale, ChildCount, Time}
    end, Scales),
    
    % Analyze scaling behavior
    ct:print("Scaling Analysis:"),
    lists:foreach(fun({Scale, Count, Time}) ->
        TimePerChild = Time / max(1, Count),
        ct:print("  ~p children: ~.2f μs total, ~.2f μs per child", 
                [Count, Time, TimePerChild])
    end, ScaleResults).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Generate shallow hierarchical test data
generate_shallow_data(Count) ->
    lists:flatten([
        {<<"/users/", (integer_to_binary(N))/binary>>, <<"user_", (integer_to_binary(N))/binary, "_data">>},
        {<<"/groups/", (integer_to_binary(N))/binary>>, <<"group_", (integer_to_binary(N))/binary, "_data">>},
        {<<"/settings/", (integer_to_binary(N))/binary>>, <<"settings_", (integer_to_binary(N))/binary, "_data">>}
        || N <- lists:seq(1, Count)
    ]).

%% @doc Generate deep hierarchical test data
generate_deep_data(Count) ->
    Depts = [<<"engineering">>, <<"marketing">>, <<"sales">>],
    Teams = [<<"backend">>, <<"frontend">>, <<"mobile">>],
    
    lists:flatten([
        begin
            DeptBin = lists:nth((N rem 3) + 1, Depts),
            TeamBin = lists:nth((N rem 3) + 1, Teams),
            UserBin = integer_to_binary(N),
            [
                {<<"/org/", DeptBin/binary, "/", TeamBin/binary, "/user", UserBin/binary, "/profile">>, 
                 <<"profile_data_", UserBin/binary>>},
                {<<"/org/", DeptBin/binary, "/", TeamBin/binary, "/user", UserBin/binary, "/settings">>, 
                 <<"settings_data_", UserBin/binary>>},
                {<<"/org/", DeptBin/binary, "/", TeamBin/binary, "/user", UserBin/binary, "/activity/today">>, 
                 <<"activity_data_", UserBin/binary>>}
            ]
        end
        || N <- lists:seq(1, Count)
    ]).

%% @doc Generate sparse test data
generate_sparse_data(PrefixCount) ->
    lists:flatten([
        begin
            PrefixBin = integer_to_binary(N),
            ChildCount = (N rem 3) + 1, % 1-3 children each
            [
                {<<"/sparse_", PrefixBin/binary, "/child", (integer_to_binary(C))/binary>>, 
                 <<"sparse_data_", PrefixBin/binary, "_", (integer_to_binary(C))/binary>>}
                || C <- lists:seq(1, ChildCount)
            ]
        end
        || N <- lists:seq(1, PrefixCount)
    ]).

%% @doc Generate dense test data
generate_dense_data(PrefixCount, ChildrenPerPrefix) ->
    lists:flatten([
        [
            {<<"/dense_", (integer_to_binary(P))/binary, "/item", (integer_to_binary(C))/binary>>, 
             <<"dense_data_", (integer_to_binary(P))/binary, "_", (integer_to_binary(C))/binary>>}
            || C <- lists:seq(1, ChildrenPerPrefix)
        ]
        || P <- lists:seq(1, PrefixCount)
    ]).

%% @doc Generate scaling test data
generate_scaling_data(Scale, ChildCount) ->
    [
        {<<"/scale_", (integer_to_binary(Scale))/binary, "/item", (integer_to_binary(C))/binary>>, 
         <<"scale_data_", (integer_to_binary(Scale))/binary, "_", (integer_to_binary(C))/binary>>}
        || C <- lists:seq(1, ChildCount)
    ].

%% @doc Write test data to database
write_test_data(DB, TestData) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun({Key, Value}) ->
        elmdb:put(DB, Key, Value)
    end, TestData),
    
    % Flush to ensure all data is written
    elmdb:flush(DB),
    
    EndTime = erlang:monotonic_time(microsecond),
    WriteTime = (EndTime - StartTime) / 1000000,
    
    ct:print("  Wrote ~p records in ~.2f seconds", [length(TestData), WriteTime]).

%% @doc Benchmark list operations with multiple iterations
benchmark_list_operations(DB, Prefixes, Iterations) ->
    lists:map(fun(Prefix) ->
        Times = lists:map(fun(_) ->
            StartTime = erlang:monotonic_time(microsecond),
            Result = elmdb:list(DB, Prefix),
            EndTime = erlang:monotonic_time(microsecond),
            
            Time = EndTime - StartTime,
            ChildCount = case Result of
                {ok, Children} -> length(Children);
                not_found -> 0;
                _ -> 0
            end,
            
            {Time, ChildCount, Result}
        end, lists:seq(1, Iterations)),
        
        AvgTime = lists:sum([T || {T, _, _} <- Times]) / Iterations,
        ChildCount = case Times of
            [{_, C, _} | _] -> C;
            [] -> 0
        end,
        
        {Prefix, ChildCount, AvgTime, Times}
    end, Prefixes).

%% @doc Print detailed benchmark results
print_detailed_results(Results) ->
    ct:print("  Detailed Results:"),
    lists:foreach(fun({Prefix, ChildCount, AvgTime, _}) ->
        OpsPerSec = 1000000 / AvgTime,
        ct:print("    ~p (~p children): ~.2f μs avg, ~.2f ops/sec", 
                [Prefix, ChildCount, AvgTime, OpsPerSec])
    end, Results).