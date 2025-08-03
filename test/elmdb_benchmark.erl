%%%-------------------------------------------------------------------
%%% @doc
%%% ElmDB Performance Benchmark Suite
%%% 
%%% This module provides comprehensive benchmarks for ElmDB operations
%%% including large-scale write/read performance and hierarchical key operations.
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_benchmark).

%% CT exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    bench_100k_writes/1,
    bench_100k_writes_batch/1,
    bench_100k_reads/1,
    bench_hierarchical_keys/1,
    bench_concurrent_access/1
]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        bench_100k_writes,
        bench_100k_writes_batch,
        bench_100k_reads,
        bench_hierarchical_keys,
        bench_concurrent_access
    ].

init_per_suite(Config) ->
    % Ensure elmdb application is available
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
    % Create unique test directory for each test case
    TestDir = filename:join([?config(priv_dir, Config), atom_to_list(TestCase)]),
    ok = filelib:ensure_dir(filename:join(TestDir, "dummy")),
    
    % Try to initialize environment with performance flags
    case catch elmdb:env_open(TestDir, [{map_size, 1024*1024*1024}, no_sync, no_mem_init, write_map]) of  % 1GB
        {ok, Env} ->
            ct:print("Environment opened for ~p: ~p", [TestCase, Env]),
            case catch elmdb:db_open(Env, [create]) of
                {ok, DB} ->
                    ct:print("Database opened for ~p: ~p", [TestCase, DB]),
                    [{test_dir, TestDir}, {env, Env}, {db, DB} | Config];
                {error, Reason} ->
                    ct:print("Failed to open database: ~p", [Reason]),
                    ct:fail("Database open failed: ~p", [Reason]);
                Error ->
                    ct:print("Database open crashed: ~p", [Error]),
                    ct:fail("Database open crashed: ~p", [Error])
            end;
        {error, Reason} ->
            ct:print("Failed to open environment: ~p", [Reason]),
            ct:fail("Environment open failed: ~p", [Reason]);
        Error ->
            ct:print("Environment open crashed: ~p", [Error]),
            % Skip test if NIF isn't loaded properly
            {skip, "NIF not loaded properly"}
    end.

end_per_testcase(_TestCase, Config) ->
    % Clean up
    case ?config(env, Config) of
        undefined -> ok;
        Env -> 
            catch elmdb:env_close(Env)
    end,
    
    % Remove test directory
    case ?config(test_dir, Config) of
        undefined -> ok;
        TestDir -> 
            file:del_dir_r(TestDir)
    end,
    ok.

%%%===================================================================
%%% Benchmark Tests
%%%===================================================================

%% @doc Benchmark writing 100,000 records with performance metrics
bench_100k_writes(Config) ->
    DB = ?config(db, Config),
    RecordCount = 100000,
    
    ct:print("Starting benchmark: Writing ~p records", [RecordCount]),
    
    % Record start time
    StartTime = erlang:monotonic_time(microsecond),
    
    % Write records with keys like <<"key", N:32>> and values like <<"value", N:32>>
    WriteResults = write_records(DB, RecordCount),
    
    % Record end time
    EndTime = erlang:monotonic_time(microsecond),
    
    % Calculate performance metrics
    TotalTime = EndTime - StartTime,
    TimeInSeconds = TotalTime / 1000000,
    RecordsPerSecond = RecordCount / TimeInSeconds,
    
    ct:print("Write Performance Results:"),
    ct:print("  Records written: ~p", [RecordCount]),
    ct:print("  Total time: ~.2f seconds", [TimeInSeconds]),
    ct:print("  Records/second: ~.2f", [RecordsPerSecond]),
    ct:print("  Microseconds per record: ~.2f", [TotalTime / RecordCount]),
    
    % Analyze write results
    case WriteResults of
        {ok, SuccessCount, Errors} ->
            ct:print("  Successful writes: ~p", [SuccessCount]),
            case Errors of
                [] -> 
                    ct:print("  No errors occurred");
                _ ->
                    ct:print("  Errors: ~p", [length(Errors)]),
                    ct:print("  First 5 errors: ~p", [lists:sublist(Errors, 5)])
            end;
        {error, WriteReason} ->
            ct:fail("Write benchmark failed: ~p", [WriteReason])
    end,
    
    % Store results in config for next test
    [{write_time, TimeInSeconds}, {write_rate, RecordsPerSecond} | Config].

%% @doc Benchmark writing 100,000 records using batch operations
bench_100k_writes_batch(Config) ->
    DB = ?config(db, Config),
    RecordCount = 100000,
    BatchSize = 1000,  % Process in batches of 1000
    
    ct:print("Starting batch benchmark: Writing ~p records in batches of ~p", [RecordCount, BatchSize]),
    
    % Generate all key-value pairs
    KeyValuePairs = generate_key_value_pairs(RecordCount),
    
    % Split into batches
    Batches = split_into_batches(KeyValuePairs, BatchSize),
    
    % Record start time
    StartTime = erlang:monotonic_time(microsecond),
    
    % Write batches
    BatchResults = write_batches(DB, Batches),
    
    % Record end time
    EndTime = erlang:monotonic_time(microsecond),
    
    % Calculate performance metrics
    TotalTime = EndTime - StartTime,
    TimeInSeconds = TotalTime / 1000000,
    RecordsPerSecond = RecordCount / TimeInSeconds,
    
    ct:print("Batch Write Performance Results:"),
    ct:print("  Records written: ~p", [RecordCount]),
    ct:print("  Batch size: ~p", [BatchSize]),
    ct:print("  Number of batches: ~p", [length(Batches)]),
    ct:print("  Total time: ~.2f seconds", [TimeInSeconds]),
    ct:print("  Records/second: ~.2f", [RecordsPerSecond]),
    ct:print("  Microseconds per record: ~.2f", [TotalTime / RecordCount]),
    
    % Analyze batch results
    case BatchResults of
        {ok, SuccessCount, Errors} ->
            ct:print("  Successful batch writes: ~p", [SuccessCount]),
            case Errors of
                [] -> 
                    ct:print("  No errors occurred");
                _ ->
                    ct:print("  Errors: ~p", [length(Errors)]),
                    ct:print("  First 5 errors: ~p", [lists:sublist(Errors, 5)])
            end;
        {error, BatchReason} ->
            ct:fail("Batch write benchmark failed: ~p", [BatchReason])
    end,
    
    % Store results in config for comparison
    [{batch_write_time, TimeInSeconds}, {batch_write_rate, RecordsPerSecond} | Config].

%% @doc Benchmark reading 100,000 records with performance metrics
bench_100k_reads(Config) ->
    DB = ?config(db, Config),
    RecordCount = 100000,
    
    % First ensure we have data to read (write if not already done)
    case ?config(write_time, Config) of
        undefined ->
            ct:print("Writing test data first..."),
            {ok, _, _} = write_records(DB, RecordCount);
        _ ->
            ct:print("Using existing test data")
    end,
    
    ct:print("Starting benchmark: Reading ~p records", [RecordCount]),
    
    % Record start time
    StartTime = erlang:monotonic_time(microsecond),
    
    % Read all records
    ReadResults = read_records(DB, RecordCount),
    
    % Record end time
    EndTime = erlang:monotonic_time(microsecond),
    
    % Calculate performance metrics
    TotalTime = EndTime - StartTime,
    TimeInSeconds = TotalTime / 1000000,
    RecordsPerSecond = RecordCount / TimeInSeconds,
    
    ct:print("Read Performance Results:"),
    ct:print("  Records read: ~p", [RecordCount]),
    ct:print("  Total time: ~.2f seconds", [TimeInSeconds]),
    ct:print("  Records/second: ~.2f", [RecordsPerSecond]),
    ct:print("  Microseconds per record: ~.2f", [TotalTime / RecordCount]),
    
    % Analyze read results
    case ReadResults of
        {ok, SuccessCount, NotFoundCount, Errors} ->
            ct:print("  Successful reads: ~p", [SuccessCount]),
            ct:print("  Not found: ~p", [NotFoundCount]),
            case Errors of
                [] -> 
                    ct:print("  No errors occurred");
                _ ->
                    ct:print("  Errors: ~p", [length(Errors)]),
                    ct:print("  First 5 errors: ~p", [lists:sublist(Errors, 5)])
            end;
        {error, ReadReason} ->
            ct:fail("Read benchmark failed: ~p", [ReadReason])
    end,
    
    % Compare with write performance
    case ?config(write_rate, Config) of
        undefined -> ok;
        WriteRate ->
            Ratio = RecordsPerSecond / WriteRate,
            ct:print("Read/Write Performance Ratio: ~.2fx", [Ratio]),
            if 
                Ratio > 1.0 -> ct:print("  Reads are faster than writes");
                Ratio < 1.0 -> ct:print("  Writes are faster than reads");
                true -> ct:print("  Read and write performance are similar")
            end
    end.

%% @doc Benchmark hierarchical key operations and list performance
bench_hierarchical_keys(Config) ->
    DB = ?config(db, Config),
    
    ct:print("Starting hierarchical key benchmark"),
    
    % Create hierarchical test data
    % Structure: /users/N/profile, /users/N/settings, /groups/N/members, etc.
    TestData = generate_hierarchical_data(1000), % 1000 users, multiple keys each
    
    ct:print("Writing ~p hierarchical records", [length(TestData)]),
    
    % Write hierarchical data
    WriteStartTime = erlang:monotonic_time(microsecond),
    WriteResult = write_hierarchical_records(DB, TestData),
    WriteEndTime = erlang:monotonic_time(microsecond),
    
    WriteTime = (WriteEndTime - WriteStartTime) / 1000000,
    ct:print("Hierarchical write time: ~.2f seconds", [WriteTime]),
    
    case WriteResult of
        {ok, WriteCount, WriteErrors} ->
            ct:print("  Successfully wrote ~p records", [WriteCount]),
            case WriteErrors of
                [] -> ok;
                _ -> ct:print("  Write errors: ~p", [length(WriteErrors)])
            end;
        {error, HierReason} ->
            ct:fail("Hierarchical write failed: ~p", [HierReason])
    end,
    
    % Test list operations on different prefixes
    Prefixes = [
        <<"/users">>,
        <<"/users/1">>,
        <<"/users/100">>,
        <<"/groups">>,
        <<"/groups/1">>
    ],
    
    ct:print("Testing list operations on prefixes"),
    
    ListResults = lists:map(fun(Prefix) ->
        StartTime = erlang:monotonic_time(microsecond),
        Result = elmdb:list(DB, Prefix),
        EndTime = erlang:monotonic_time(microsecond),
        
        Time = EndTime - StartTime,
        case Result of
            {ok, Children} ->
                ct:print("  Prefix ~p: ~p children in ~p μs", 
                        [Prefix, length(Children), Time]),
                {Prefix, length(Children), Time, ok};
            not_found ->
                ct:print("  Prefix ~p: not found in ~p μs", [Prefix, Time]),
                {Prefix, 0, Time, not_found};
            {error, Reason} ->
                ct:print("  Prefix ~p: error ~p in ~p μs", [Prefix, Reason, Time]),
                {Prefix, 0, Time, {error, Reason}}
        end
    end, Prefixes),
    
    % Calculate average list operation time
    ListTimes = [Time || {_, _, Time, _} <- ListResults],
    AvgListTime = lists:sum(ListTimes) / length(ListTimes),
    ct:print("Average list operation time: ~.2f μs", [AvgListTime]).

%% @doc Benchmark concurrent access patterns
bench_concurrent_access(Config) ->
    DB = ?config(db, Config),
    
    ct:print("Starting concurrent access benchmark"),
    
    % Prepare test data
    RecordsPerProcess = 1000,
    ProcessCount = 10,
    TotalRecords = RecordsPerProcess * ProcessCount,
    
    ct:print("Testing ~p processes writing ~p records each", [ProcessCount, RecordsPerProcess]),
    
    % Start concurrent writers
    StartTime = erlang:monotonic_time(microsecond),
    
    WriterPids = lists:map(fun(ProcessId) ->
        spawn_link(fun() ->
            write_concurrent_records(DB, ProcessId, RecordsPerProcess)
        end)
    end, lists:seq(1, ProcessCount)),
    
    % Wait for all writers to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, normal} -> ok;
            {'EXIT', Pid, Reason} -> 
                ct:print("Writer process ~p failed: ~p", [Pid, Reason])
        after 30000 ->
            ct:print("Writer process ~p timed out", [Pid])
        end
    end, WriterPids),
    
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = (EndTime - StartTime) / 1000000,
    RecordsPerSecond = TotalRecords / TotalTime,
    
    ct:print("Concurrent Write Results:"),
    ct:print("  Total records: ~p", [TotalRecords]),
    ct:print("  Total time: ~.2f seconds", [TotalTime]),
    ct:print("  Records/second: ~.2f", [RecordsPerSecond]),
    ct:print("  Processes: ~p", [ProcessCount]).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Write N records with performance tracking
write_records(DB, Count) ->
    write_records(DB, 1, Count, 0, []).

write_records(_DB, N, Count, SuccessCount, Errors) when N > Count ->
    {ok, SuccessCount, lists:reverse(Errors)};
write_records(DB, N, Count, SuccessCount, Errors) ->
    Key = <<"key", N:32>>,
    Value = <<"value", N:32>>,
    
    case catch elmdb:put(DB, Key, Value) of
        ok ->
            write_records(DB, N + 1, Count, SuccessCount + 1, Errors);
        {error, Reason} ->
            write_records(DB, N + 1, Count, SuccessCount, [{N, Reason} | Errors]);
        Error ->
            write_records(DB, N + 1, Count, SuccessCount, [{N, Error} | Errors])
    end.

%% @doc Read N records with performance tracking
read_records(DB, Count) ->
    read_records(DB, 1, Count, 0, 0, []).

read_records(_DB, N, Count, SuccessCount, NotFoundCount, Errors) when N > Count ->
    {ok, SuccessCount, NotFoundCount, lists:reverse(Errors)};
read_records(DB, N, Count, SuccessCount, NotFoundCount, Errors) ->
    Key = <<"key", N:32>>,
    
    case catch elmdb:get(DB, Key) of
        {ok, _Value} ->
            read_records(DB, N + 1, Count, SuccessCount + 1, NotFoundCount, Errors);
        not_found ->
            read_records(DB, N + 1, Count, SuccessCount, NotFoundCount + 1, Errors);
        {error, Reason} ->
            read_records(DB, N + 1, Count, SuccessCount, NotFoundCount, [{N, Reason} | Errors]);
        Error ->
            read_records(DB, N + 1, Count, SuccessCount, NotFoundCount, [{N, Error} | Errors])
    end.

%% @doc Generate hierarchical test data
generate_hierarchical_data(UserCount) ->
    Users = lists:seq(1, UserCount),
    lists:flatten(lists:map(fun(UserId) ->
        UserIdBin = integer_to_binary(UserId),
        [
            {<<"/users/", UserIdBin/binary, "/profile">>, <<"User ", UserIdBin/binary, " profile data">>},
            {<<"/users/", UserIdBin/binary, "/settings">>, <<"User ", UserIdBin/binary, " settings">>},
            {<<"/users/", UserIdBin/binary, "/activity">>, <<"User ", UserIdBin/binary, " activity log">>}
        ]
    end, Users)) ++
    % Add some group data
    lists:flatten(lists:map(fun(GroupId) ->
        GroupIdBin = integer_to_binary(GroupId),
        [
            {<<"/groups/", GroupIdBin/binary, "/info">>, <<"Group ", GroupIdBin/binary, " information">>},
            {<<"/groups/", GroupIdBin/binary, "/members">>, <<"Group ", GroupIdBin/binary, " member list">>}
        ]
    end, lists:seq(1, UserCount div 10))).

%% @doc Write hierarchical records
write_hierarchical_records(DB, TestData) ->
    write_hierarchical_records(DB, TestData, 0, []).

write_hierarchical_records(_DB, [], SuccessCount, Errors) ->
    {ok, SuccessCount, lists:reverse(Errors)};
write_hierarchical_records(DB, [{Key, Value} | Rest], SuccessCount, Errors) ->
    case catch elmdb:put(DB, Key, Value) of
        ok ->
            write_hierarchical_records(DB, Rest, SuccessCount + 1, Errors);
        Error ->
            write_hierarchical_records(DB, Rest, SuccessCount, [{Key, Error} | Errors])
    end.

%% @doc Write records concurrently from a specific process
write_concurrent_records(DB, ProcessId, Count) ->
    lists:foreach(fun(N) ->
        Key = <<"proc", ProcessId:16, "_key", N:32>>,
        Value = <<"proc", ProcessId:16, "_value", N:32>>,
        elmdb:put(DB, Key, Value)
    end, lists:seq(1, Count)).

%% @doc Generate key-value pairs for batch operations
generate_key_value_pairs(Count) ->
    lists:map(fun(N) ->
        Key = <<"key", N:32>>,
        Value = <<"value", N:32>>,
        {Key, Value}
    end, lists:seq(1, Count)).

%% @doc Split key-value pairs into batches
split_into_batches(KeyValuePairs, BatchSize) ->
    split_into_batches(KeyValuePairs, BatchSize, []).

split_into_batches([], _BatchSize, Acc) ->
    lists:reverse(Acc);
split_into_batches(KeyValuePairs, BatchSize, Acc) ->
    {Batch, Remaining} = lists:split(min(BatchSize, length(KeyValuePairs)), KeyValuePairs),
    split_into_batches(Remaining, BatchSize, [Batch | Acc]).

%% @doc Write batches of key-value pairs
write_batches(DB, Batches) ->
    write_batches(DB, Batches, 0, []).

write_batches(_DB, [], SuccessCount, Errors) ->
    {ok, SuccessCount, lists:reverse(Errors)};
write_batches(DB, [Batch | Rest], SuccessCount, Errors) ->
    case catch elmdb:put_batch(DB, Batch) of
        ok ->
            write_batches(DB, Rest, SuccessCount + 1, Errors);
        {ok, _BatchSuccessCount, BatchErrors} ->
            write_batches(DB, Rest, SuccessCount + 1, BatchErrors ++ Errors);
        {error, Reason} ->
            write_batches(DB, Rest, SuccessCount, [{batch_error, Reason} | Errors]);
        Error ->
            write_batches(DB, Rest, SuccessCount, [{batch_error, Error} | Errors])
    end.