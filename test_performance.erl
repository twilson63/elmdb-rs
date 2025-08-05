#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% Performance test script for ElmDB
%%% Tests both individual writes and batch writes with performance flags
%%%-------------------------------------------------------------------

main(_) ->
    % Ensure the elmdb module is available
    code:add_path("_build/default/lib/elmdb/ebin"),
    
    TestDir = "/tmp/elmdb_perf_test",
    
    % Clean up any existing test directory
    file:del_dir_r(TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/dummy"),
    
    io:format("~n=== ElmDB Performance Test ===~n"),
    io:format("Test directory: ~s~n", [TestDir]),
    
    % Test 1: Individual writes with performance flags
    io:format("~n--- Test 1: Individual writes with performance flags ---~n"),
    test_individual_writes(TestDir ++ "_individual"),
    
    % Test 2: Batch writes with performance flags
    io:format("~n--- Test 2: Batch writes with performance flags ---~n"),
    test_batch_writes(TestDir ++ "_batch"),
    
    % Test 3: Individual writes without performance flags (for comparison)
    io:format("~n--- Test 3: Individual writes without performance flags ---~n"),
    test_individual_writes_no_flags(TestDir ++ "_no_flags"),
    
    io:format("~n=== Performance test completed ===~n").

test_individual_writes(TestDir) ->
    RecordCount = 50000,
    
    % Open environment with performance flags
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 1024*1024*1024}, no_sync, no_mem_init, write_map]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    io:format("Writing ~p records individually with performance flags...~n", [RecordCount]),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(N) ->
        Key = <<"key", N:32>>,
        Value = <<"value", N:32>>,
        ok = elmdb:put(DB, Key, Value)
    end, lists:seq(1, RecordCount)),
    
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = (EndTime - StartTime) / 1000000,
    RecordsPerSecond = RecordCount / TotalTime,
    
    io:format("Results:~n"),
    io:format("  Records: ~p~n", [RecordCount]),
    io:format("  Time: ~.3f seconds~n", [TotalTime]),
    io:format("  Rate: ~.0f records/second~n", [RecordsPerSecond]),
    
    % Verify data
    verify_data(DB, RecordCount),
    
    elmdb:env_close(Env).

test_batch_writes(TestDir) ->
    RecordCount = 50000,
    BatchSize = 1000,
    
    % Open environment with performance flags
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 1024*1024*1024}, no_sync, no_mem_init, write_map]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Prepare batches
    KeyValuePairs = lists:map(fun(N) ->
        Key = <<"key", N:32>>,
        Value = <<"value", N:32>>,
        {Key, Value}
    end, lists:seq(1, RecordCount)),
    
    Batches = split_into_batches(KeyValuePairs, BatchSize),
    
    io:format("Writing ~p records in ~p batches of ~p with performance flags...~n", 
             [RecordCount, length(Batches), BatchSize]),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(Batch) ->
        ok = elmdb:put_batch(DB, Batch)
    end, Batches),
    
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = (EndTime - StartTime) / 1000000,
    RecordsPerSecond = RecordCount / TotalTime,
    
    io:format("Results:~n"),
    io:format("  Records: ~p~n", [RecordCount]),
    io:format("  Batches: ~p~n", [length(Batches)]),
    io:format("  Time: ~.3f seconds~n", [TotalTime]),
    io:format("  Rate: ~.0f records/second~n", [RecordsPerSecond]),
    
    % Verify data
    verify_data(DB, RecordCount),
    
    elmdb:env_close(Env).

test_individual_writes_no_flags(TestDir) ->
    RecordCount = 10000,  % Smaller test since this will be slower
    
    % Open environment without performance flags
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 1024*1024*1024}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    io:format("Writing ~p records individually without performance flags...~n", [RecordCount]),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(N) ->
        Key = <<"key", N:32>>,
        Value = <<"value", N:32>>,
        ok = elmdb:put(DB, Key, Value)
    end, lists:seq(1, RecordCount)),
    
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = (EndTime - StartTime) / 1000000,
    RecordsPerSecond = RecordCount / TotalTime,
    
    io:format("Results:~n"),
    io:format("  Records: ~p~n", [RecordCount]),
    io:format("  Time: ~.3f seconds~n", [TotalTime]),
    io:format("  Rate: ~.0f records/second~n", [RecordsPerSecond]),
    
    % Verify data
    verify_data(DB, RecordCount),
    
    elmdb:env_close(Env).

verify_data(DB, Count) ->
    io:format("Verifying data integrity...~n"),
    
    VerifyCount = min(1000, Count),  % Verify first 1000 records
    Verified = lists:foldl(fun(N, Acc) ->
        Key = <<"key", N:32>>,
        ExpectedValue = <<"value", N:32>>,
        case elmdb:get(DB, Key) of
            {ok, ExpectedValue} -> Acc + 1;
            {ok, Other} ->
                io:format("  ERROR: Key ~p has value ~p, expected ~p~n", [Key, Other, ExpectedValue]),
                Acc;
            not_found ->
                io:format("  ERROR: Key ~p not found~n", [Key]),
                Acc;
            Error ->
                io:format("  ERROR: Key ~p caused error ~p~n", [Key, Error]),
                Acc
        end
    end, 0, lists:seq(1, VerifyCount)),
    
    if 
        Verified =:= VerifyCount ->
            io:format("  Verification successful: ~p/~p records verified~n", [Verified, VerifyCount]);
        true ->
            io:format("  Verification FAILED: only ~p/~p records verified~n", [Verified, VerifyCount])
    end.

split_into_batches(List, BatchSize) ->
    split_into_batches(List, BatchSize, []).

split_into_batches([], _BatchSize, Acc) ->
    lists:reverse(Acc);
split_into_batches(List, BatchSize, Acc) ->
    {Batch, Remaining} = lists:split(min(BatchSize, length(List)), List),
    split_into_batches(Remaining, BatchSize, [Batch | Acc]).