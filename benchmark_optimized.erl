#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc
%%% ElmDB Optimized Performance Benchmark
%%% 
%%% A simple benchmark script that tests elmdb-rs with optimized settings:
%%% - no_sync: Don't flush system buffers to disk when committing
%%% - no_mem_init: Don't initialize malloc'd memory before writing
%%% - write_map: Use writeable memory map for better performance
%%% 
%%% Usage: escript benchmark_optimized.erl
%%% @end
%%%-------------------------------------------------------------------

main(_Args) ->
    io:format("~n=== ElmDB Optimized Performance Benchmark ===~n~n"),
    
    % Load the elmdb module
    case code:ensure_loaded(elmdb) of
        {module, elmdb} ->
            run_benchmark();
        {error, Reason} ->
            io:format("ERROR: Failed to load elmdb module: ~p~n", [Reason]),
            io:format("Please ensure you're running from the elmdb-rs project directory.~n"),
            halt(1)
    end.

run_benchmark() ->
    % Configuration
    TestDir = "/tmp/elmdb_benchmark_" ++ integer_to_list(erlang:system_time()),
    RecordCount = 100000,
    
    % Ensure test directory exists
    ok = filelib:ensure_dir(filename:join(TestDir, "dummy")),
    
    io:format("Configuration:~n"),
    io:format("  Test directory: ~s~n", [TestDir]),
    io:format("  Record count: ~p~n", [RecordCount]),
    io:format("  Optimizations: no_sync, no_mem_init, write_map~n~n"),
    
    try
        % Open environment with optimized flags
        {ok, Env} = elmdb:env_open(TestDir, [
            {map_size, 2 * 1024 * 1024 * 1024},  % 2GB map size
            no_sync,        % Don't sync to disk (faster writes)
            no_mem_init,    % Don't zero-initialize memory
            write_map       % Use writeable memory map
        ]),
        
        % Open database
        {ok, DB} = elmdb:db_open(Env, [create]),
        
        io:format("Environment and database opened successfully.~n~n"),
        
        % Run write benchmark
        WriteResults = benchmark_writes(DB, RecordCount),
        display_write_results(WriteResults),
        
        % Run read benchmark
        ReadResults = benchmark_reads(DB, RecordCount),
        display_read_results(ReadResults),
        
        % Display summary
        display_summary(WriteResults, ReadResults),
        
        % Cleanup
        elmdb:env_close(Env),
        file:del_dir_r(TestDir),
        io:format("~nBenchmark completed successfully!~n")
        
    catch
        error:{badmatch, {error, Reason}} ->
            io:format("ERROR: Benchmark failed: ~p~n", [Reason]),
            file:del_dir_r(TestDir),
            halt(1);
        Class:Reason:Stacktrace ->
            io:format("ERROR: Unexpected error: ~p:~p~n", [Class, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            file:del_dir_r(TestDir),
            halt(1)
    end.

%% @doc Benchmark writing records with optimized settings
benchmark_writes(DB, RecordCount) ->
    io:format("--- WRITE BENCHMARK ---~n"),
    io:format("Writing ~p records...~n", [RecordCount]),
    
    % Record start time with high precision
    StartTime = erlang:monotonic_time(microsecond),
    
    % Write records
    {SuccessCount, ErrorCount} = write_records_optimized(DB, RecordCount),
    
    % Record end time
    EndTime = erlang:monotonic_time(microsecond),
    
    % Calculate metrics
    TotalTimeUs = EndTime - StartTime,
    TotalTimeMs = TotalTimeUs / 1000,
    TotalTimeS = TotalTimeUs / 1000000,
    
    RecordsPerSecond = case TotalTimeS > 0 of
        true -> RecordCount / TotalTimeS;
        false -> infinity
    end,
    
    UsPerRecord = case RecordCount > 0 of
        true -> TotalTimeUs / RecordCount;
        false -> 0
    end,
    
    #{
        total_records => RecordCount,
        success_count => SuccessCount,
        error_count => ErrorCount,
        total_time_us => TotalTimeUs,
        total_time_ms => TotalTimeMs,
        total_time_s => TotalTimeS,
        records_per_second => RecordsPerSecond,
        microseconds_per_record => UsPerRecord
    }.

%% @doc Benchmark reading records with optimized settings
benchmark_reads(DB, RecordCount) ->
    io:format("--- READ BENCHMARK ---~n"),
    io:format("Reading ~p records...~n", [RecordCount]),
    
    % Record start time with high precision
    StartTime = erlang:monotonic_time(microsecond),
    
    % Read records
    {SuccessCount, NotFoundCount, ErrorCount} = read_records_optimized(DB, RecordCount),
    
    % Record end time
    EndTime = erlang:monotonic_time(microsecond),
    
    % Calculate metrics
    TotalTimeUs = EndTime - StartTime,
    TotalTimeMs = TotalTimeUs / 1000,
    TotalTimeS = TotalTimeUs / 1000000,
    
    RecordsPerSecond = case TotalTimeS > 0 of
        true -> RecordCount / TotalTimeS;
        false -> infinity
    end,
    
    UsPerRecord = case RecordCount > 0 of
        true -> TotalTimeUs / RecordCount;
        false -> 0
    end,
    
    #{
        total_records => RecordCount,
        success_count => SuccessCount,
        not_found_count => NotFoundCount,
        error_count => ErrorCount,
        total_time_us => TotalTimeUs,
        total_time_ms => TotalTimeMs,
        total_time_s => TotalTimeS,
        records_per_second => RecordsPerSecond,
        microseconds_per_record => UsPerRecord
    }.

%% @doc Write records efficiently
write_records_optimized(DB, Count) ->
    write_records_loop(DB, 1, Count, 0, 0).

write_records_loop(_DB, N, Count, SuccessCount, ErrorCount) when N > Count ->
    {SuccessCount, ErrorCount};
write_records_loop(DB, N, Count, SuccessCount, ErrorCount) ->
    Key = <<"key_", (integer_to_binary(N))/binary>>,
    Value = <<"value_", (integer_to_binary(N))/binary, "_data_", (crypto:strong_rand_bytes(16))/binary>>,
    
    case elmdb:put(DB, Key, Value) of
        ok ->
            write_records_loop(DB, N + 1, Count, SuccessCount + 1, ErrorCount);
        {error, _Reason} ->
            write_records_loop(DB, N + 1, Count, SuccessCount, ErrorCount + 1)
    end.

%% @doc Read records efficiently
read_records_optimized(DB, Count) ->
    read_records_loop(DB, 1, Count, 0, 0, 0).

read_records_loop(_DB, N, Count, SuccessCount, NotFoundCount, ErrorCount) when N > Count ->
    {SuccessCount, NotFoundCount, ErrorCount};
read_records_loop(DB, N, Count, SuccessCount, NotFoundCount, ErrorCount) ->
    Key = <<"key_", (integer_to_binary(N))/binary>>,
    
    case elmdb:get(DB, Key) of
        {ok, _Value} ->
            read_records_loop(DB, N + 1, Count, SuccessCount + 1, NotFoundCount, ErrorCount);
        not_found ->
            read_records_loop(DB, N + 1, Count, SuccessCount, NotFoundCount + 1, ErrorCount);
        {error, _Reason} ->
            read_records_loop(DB, N + 1, Count, SuccessCount, NotFoundCount, ErrorCount + 1)
    end.

%% @doc Display write benchmark results
display_write_results(#{
    total_records := TotalRecords,
    success_count := SuccessCount,
    error_count := ErrorCount,
    total_time_s := TotalTimeS,
    records_per_second := RecordsPerSecond,
    microseconds_per_record := UsPerRecord
}) ->
    io:format("Write Results:~n"),
    io:format("  Total records:     ~10w~n", [TotalRecords]),
    io:format("  Successful writes: ~10w~n", [SuccessCount]),
    io:format("  Failed writes:     ~10w~n", [ErrorCount]),
    io:format("  Total time:        ~10.3f seconds~n", [TotalTimeS]),
    io:format("  Write rate:        ~10.2f records/second~n", [RecordsPerSecond]),
    io:format("  Time per record:   ~10.2f microseconds~n", [UsPerRecord]),
    
    % Calculate throughput in common units
    MBPerSecond = (RecordsPerSecond * 50) / (1024 * 1024), % Assuming ~50 bytes per record
    io:format("  Estimated throughput: ~7.2f MB/second~n", [MBPerSecond]),
    io:format("~n").

%% @doc Display read benchmark results
display_read_results(#{
    total_records := TotalRecords,
    success_count := SuccessCount,
    not_found_count := NotFoundCount,
    error_count := ErrorCount,
    total_time_s := TotalTimeS,
    records_per_second := RecordsPerSecond,
    microseconds_per_record := UsPerRecord
}) ->
    io:format("Read Results:~n"),
    io:format("  Total records:     ~10w~n", [TotalRecords]),
    io:format("  Successful reads:  ~10w~n", [SuccessCount]),
    io:format("  Not found:         ~10w~n", [NotFoundCount]),
    io:format("  Failed reads:      ~10w~n", [ErrorCount]),
    io:format("  Total time:        ~10.3f seconds~n", [TotalTimeS]),
    io:format("  Read rate:         ~10.2f records/second~n", [RecordsPerSecond]),
    io:format("  Time per record:   ~10.2f microseconds~n", [UsPerRecord]),
    
    % Calculate throughput in common units
    MBPerSecond = (RecordsPerSecond * 50) / (1024 * 1024), % Assuming ~50 bytes per record
    io:format("  Estimated throughput: ~7.2f MB/second~n", [MBPerSecond]),
    io:format("~n").

%% @doc Display performance summary
display_summary(WriteResults, ReadResults) ->
    WriteRate = maps:get(records_per_second, WriteResults),
    ReadRate = maps:get(records_per_second, ReadResults),
    
    io:format("=== PERFORMANCE SUMMARY ===~n"),
    io:format("Write Performance: ~10.2f records/second~n", [WriteRate]),
    io:format("Read Performance:  ~10.2f records/second~n", [ReadRate]),
    
    ReadWriteRatio = case WriteRate > 0 of
        true -> ReadRate / WriteRate;
        false -> 0
    end,
    
    io:format("Read/Write Ratio:  ~10.2fx~n", [ReadWriteRatio]),
    
    % Performance assessment
    io:format("~nPerformance Assessment:~n"),
    
    case WriteRate of
        WR when WR > 100000 -> io:format("  Write: EXCELLENT (>100K records/sec)~n");
        WR when WR > 50000  -> io:format("  Write: VERY GOOD (>50K records/sec)~n");
        WR when WR > 25000  -> io:format("  Write: GOOD (>25K records/sec)~n");
        WR when WR > 10000  -> io:format("  Write: FAIR (>10K records/sec)~n");
        _                   -> io:format("  Write: NEEDS IMPROVEMENT (<10K records/sec)~n")
    end,
    
    case ReadRate of
        RR when RR > 500000 -> io:format("  Read:  EXCELLENT (>500K records/sec)~n");
        RR when RR > 250000 -> io:format("  Read:  VERY GOOD (>250K records/sec)~n");
        RR when RR > 100000 -> io:format("  Read:  GOOD (>100K records/sec)~n");
        RR when RR > 50000  -> io:format("  Read:  FAIR (>50K records/sec)~n");
        _                   -> io:format("  Read:  NEEDS IMPROVEMENT (<50K records/sec)~n")
    end,
    
    % Optimization suggestions
    io:format("~nOptimization Notes:~n"),
    io:format("  - Using no_sync: Writes don't wait for disk sync~n"),
    io:format("  - Using no_mem_init: Memory not zero-initialized~n"),
    io:format("  - Using write_map: Memory-mapped writes enabled~n"),
    io:format("  - These settings prioritize performance over durability~n").