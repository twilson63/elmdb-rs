%%%-------------------------------------------------------------------
%%% @doc
%%% Final performance comparison test suite
%%% Compares optimized vs unoptimized performance
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_final_perf_SUITE).

%% CT exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_optimized_writes/1,
    test_unoptimized_writes/1,
    test_batch_writes/1
]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_unoptimized_writes,
        test_optimized_writes,
        test_batch_writes
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
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    case ?config(test_dir, Config) of
        undefined -> ok;
        TestDir -> file:del_dir_r(TestDir)
    end,
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_unoptimized_writes(Config) ->
    TestDir = ?config(test_dir, Config),
    RecordCount = 10000,  % Smaller for unoptimized test
    
    ct:print("=== UNOPTIMIZED WRITES TEST ==="),
    ct:print("Opening environment WITHOUT performance flags"),
    
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 1024*1024*1024}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    ct:print("Writing ~p records individually (no performance flags)...", [RecordCount]),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(N) ->
        Key = <<"key", N:32>>,
        Value = <<"value", N:32>>,
        ok = elmdb:put(DB, Key, Value)
    end, lists:seq(1, RecordCount)),
    
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = (EndTime - StartTime) / 1000000,
    RecordsPerSecond = RecordCount / TotalTime,
    
    ct:print("UNOPTIMIZED RESULTS:"),
    ct:print("  Records: ~p", [RecordCount]),
    ct:print("  Time: ~.3f seconds", [TotalTime]),
    ct:print("  Rate: ~.0f records/second", [RecordsPerSecond]),
    
    elmdb:env_close(Env),
    [{unopt_rate, RecordsPerSecond} | Config].

test_optimized_writes(Config) ->
    TestDir = ?config(test_dir, Config),
    RecordCount = 50000,  % Larger for optimized test
    
    ct:print("=== OPTIMIZED WRITES TEST ==="),
    ct:print("Opening environment WITH performance flags: no_sync, no_mem_init, write_map"),
    
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 1024*1024*1024}, no_sync, no_mem_init, write_map]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    ct:print("Writing ~p records individually (with performance flags)...", [RecordCount]),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(N) ->
        Key = <<"key", N:32>>,
        Value = <<"value", N:32>>,
        ok = elmdb:put(DB, Key, Value)
    end, lists:seq(1, RecordCount)),
    
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = (EndTime - StartTime) / 1000000,
    RecordsPerSecond = RecordCount / TotalTime,
    
    ct:print("OPTIMIZED RESULTS:"),
    ct:print("  Records: ~p", [RecordCount]),
    ct:print("  Time: ~.3f seconds", [TotalTime]),
    ct:print("  Rate: ~.0f records/second", [RecordsPerSecond]),
    
    % Compare with unoptimized if available
    case ?config(unopt_rate, Config) of
        undefined -> ok;
        UnoptRate ->
            Improvement = RecordsPerSecond / UnoptRate,
            ct:print("  Improvement: ~.1fx faster than unoptimized", [Improvement])
    end,
    
    elmdb:env_close(Env),
    [{opt_rate, RecordsPerSecond} | Config].

test_batch_writes(Config) ->
    TestDir = ?config(test_dir, Config),
    RecordCount = 50000,
    BatchSize = 1000,
    
    ct:print("=== BATCH WRITES TEST ==="),
    ct:print("Opening environment WITH performance flags: no_sync, no_mem_init, write_map"),
    
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 1024*1024*1024}, no_sync, no_mem_init, write_map]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Prepare batches
    KeyValuePairs = lists:map(fun(N) ->
        Key = <<"key", N:32>>,
        Value = <<"value", N:32>>,
        {Key, Value}
    end, lists:seq(1, RecordCount)),
    
    Batches = split_into_batches(KeyValuePairs, BatchSize),
    
    ct:print("Writing ~p records in ~p batches of ~p...", [RecordCount, length(Batches), BatchSize]),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(Batch) ->
        ok = elmdb:put_batch(DB, Batch)
    end, Batches),
    
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = (EndTime - StartTime) / 1000000,
    RecordsPerSecond = RecordCount / TotalTime,
    
    ct:print("BATCH RESULTS:"),
    ct:print("  Records: ~p", [RecordCount]),
    ct:print("  Batches: ~p", [length(Batches)]),
    ct:print("  Time: ~.3f seconds", [TotalTime]),
    ct:print("  Rate: ~.0f records/second", [RecordsPerSecond]),
    
    % Compare with individual writes if available
    case ?config(opt_rate, Config) of
        undefined -> ok;
        OptRate ->
            Improvement = RecordsPerSecond / OptRate,
            ct:print("  Improvement: ~.1fx faster than individual optimized writes", [Improvement])
    end,
    
    elmdb:env_close(Env).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

split_into_batches(List, BatchSize) ->
    split_into_batches(List, BatchSize, []).

split_into_batches([], _BatchSize, Acc) ->
    lists:reverse(Acc);
split_into_batches(List, BatchSize, Acc) ->
    {Batch, Remaining} = lists:split(min(BatchSize, length(List)), List),
    split_into_batches(Remaining, BatchSize, [Batch | Acc]).