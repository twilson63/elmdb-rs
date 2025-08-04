%%%-------------------------------------------------------------------
%%% @doc
%%% Performance and stress tests for elmdb NIF
%%% Tests high-volume operations, concurrency, and performance characteristics
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_perf_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% Common Test callbacks
%%====================================================================

suite() ->
    [{timetrap, {minutes, 10}}].  % Longer timeout for performance tests

init_per_suite(Config) ->
    ok = application:start(elmdb),
    Config.

end_per_suite(_Config) ->
    application:stop(elmdb),
    ok.

init_per_testcase(TestCase, Config) ->
    % Create unique test directory for each test case
    TestDir = filename:join([?config(priv_dir, Config), 
                            atom_to_list(TestCase)]),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    % Clean up test directory
    TestDir = ?config(test_dir, Config),
    case file:del_dir_r(TestDir) of
        ok -> ok;
        {error, enoent} -> ok;
        Error -> ct:pal("Failed to clean up ~p: ~p", [TestDir, Error])
    end,
    ok.

all() ->
    [
        test_bulk_put_operations,
        test_bulk_get_operations,
        test_mixed_read_write_operations,
        test_large_value_handling,
        test_many_small_keys,
        test_sequential_vs_random_access,
        test_concurrent_readers,
        test_concurrent_writers,
        test_memory_usage_patterns,
        test_list_performance,
        test_database_growth,
        test_key_iteration_performance
    ].

%%====================================================================
%% Helper functions
%%====================================================================

setup_database(Config) ->
    setup_database(Config, []).

setup_database(Config, EnvOptions) ->
    TestDir = ?config(test_dir, Config),
    DefaultOptions = [{map_size, 104857600}], % 100MB default
    Options = DefaultOptions ++ EnvOptions,
    {ok, Env} = elmdb:env_open(TestDir, Options),
    {ok, DB} = elmdb:db_open(Env, [create]),
    {Env, DB}.

generate_test_data(Count) ->
    generate_test_data(Count, 100). % Default 100 byte values

generate_test_data(Count, ValueSize) ->
    [begin
         Key = list_to_binary(io_lib:format("key_~6..0w", [N])),
         Value = binary:copy(<<"V">>, ValueSize),
         {Key, Value}
     end || N <- lists:seq(1, Count)].

measure_time(Fun) ->
    Start = erlang:monotonic_time(microsecond),
    Result = Fun(),
    End = erlang:monotonic_time(microsecond),
    {End - Start, Result}.

%%====================================================================
%% Bulk Operations Tests
%%====================================================================

test_bulk_put_operations(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Test different batch sizes
    BatchSizes = [100, 1000, 10000],
    
    lists:foreach(fun(BatchSize) ->
        ct:pal("Testing bulk put with ~p records", [BatchSize]),
        
        TestData = generate_test_data(BatchSize),
        
        {TimeMicros, ok} = measure_time(fun() ->
            lists:foreach(fun({Key, Value}) ->
                ok = elmdb:put(DB, Key, Value)
            end, TestData)
        end),
        
        TimeSeconds = TimeMicros / 1000000,
        OpsPerSecond = BatchSize / TimeSeconds,
        
        ct:pal("Bulk put ~p records: ~.2f seconds (~.0f ops/sec)", 
               [BatchSize, TimeSeconds, OpsPerSecond]),
        
        % Verify all data was written
        lists:foreach(fun({Key, ExpectedValue}) ->
            {ok, Value} = elmdb:get(DB, Key),
            ExpectedValue = Value
        end, lists:sublist(TestData, 10)) % Check first 10
        
    end, BatchSizes),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

test_bulk_get_operations(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Setup test data
    BatchSize = 10000,
    TestData = generate_test_data(BatchSize),
    
    % Put all data first
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    ct:pal("Testing bulk get with ~p records", [BatchSize]),
    
    % Measure get performance
    Keys = [Key || {Key, _Value} <- TestData],
    
    {TimeMicros, _Results} = measure_time(fun() ->
        [elmdb:get(DB, Key) || Key <- Keys]
    end),
    
    TimeSeconds = TimeMicros / 1000000,
    OpsPerSecond = BatchSize / TimeSeconds,
    
    ct:pal("Bulk get ~p records: ~.2f seconds (~.0f ops/sec)", 
           [BatchSize, TimeSeconds, OpsPerSecond]),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

test_mixed_read_write_operations(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Mix of read and write operations
    NumOperations = 5000,
    
    % Pre-populate some data
    InitialData = generate_test_data(1000),
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, InitialData),
    
    ct:pal("Testing mixed read/write operations (~p ops)", [NumOperations]),
    
    {TimeMicros, _} = measure_time(fun() ->
        lists:foreach(fun(N) ->
            case N rem 3 of
                0 -> % Write operation
                    Key = list_to_binary(io_lib:format("mixed_key_~w", [N])),
                    Value = list_to_binary(io_lib:format("mixed_value_~w", [N])),
                    ok = elmdb:put(DB, Key, Value);
                _ -> % Read operation
                    KeyNum = (N rem 1000) + 1,
                    Key = list_to_binary(io_lib:format("key_~6..0w", [KeyNum])),
                    _ = elmdb:get(DB, Key)
            end
        end, lists:seq(1, NumOperations))
    end),
    
    TimeSeconds = TimeMicros / 1000000,
    OpsPerSecond = NumOperations / TimeSeconds,
    
    ct:pal("Mixed operations: ~.2f seconds (~.0f ops/sec)", 
           [TimeSeconds, OpsPerSecond]),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

%%====================================================================
%% Large Data Tests
%%====================================================================

test_large_value_handling(Config) ->
    {Env, DB} = setup_database(Config, [{map_size, 1073741824}]), % 1GB
    
    % Test various large value sizes
    ValueSizes = [1024, 10240, 102400, 1048576], % 1KB, 10KB, 100KB, 1MB
    
    lists:foreach(fun(ValueSize) ->
        ct:pal("Testing large values of size ~p bytes", [ValueSize]),
        
        Key = list_to_binary(io_lib:format("large_~w", [ValueSize])),
        Value = binary:copy(<<"X">>, ValueSize),
        
        {PutTimeMicros, ok} = measure_time(fun() ->
            elmdb:put(DB, Key, Value)
        end),
        
        {GetTimeMicros, {ok, RetrievedValue}} = measure_time(fun() ->
            elmdb:get(DB, Key)
        end),
        
        Value = RetrievedValue,
        
        PutTimeMs = PutTimeMicros / 1000,
        GetTimeMs = GetTimeMicros / 1000,
        
        ct:pal("Value size ~p bytes: Put ~.2f ms, Get ~.2f ms", 
               [ValueSize, PutTimeMs, GetTimeMs])
        
    end, ValueSizes),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

test_many_small_keys(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Test with many small key-value pairs
    NumKeys = 50000,
    SmallValue = <<"small_value">>,
    
    ct:pal("Testing ~p small key-value pairs", [NumKeys]),
    
    {TimeMicros, _} = measure_time(fun() ->
        lists:foreach(fun(N) ->
            Key = list_to_binary(io_lib:format("small_~w", [N])),
            ok = elmdb:put(DB, Key, SmallValue)
        end, lists:seq(1, NumKeys))
    end),
    
    TimeSeconds = TimeMicros / 1000000,
    OpsPerSecond = NumKeys / TimeSeconds,
    
    ct:pal("Small keys insert: ~.2f seconds (~.0f ops/sec)", 
           [TimeSeconds, OpsPerSecond]),
    
    % Test random access
    RandomKeys = [list_to_binary(io_lib:format("small_~w", [rand:uniform(NumKeys)])) 
                  || _ <- lists:seq(1, 1000)],
    
    {RandomTimeMicros, _} = measure_time(fun() ->
        [elmdb:get(DB, Key) || Key <- RandomKeys]
    end),
    
    RandomTimeMs = RandomTimeMicros / 1000,
    RandomOpsPerSec = 1000 / (RandomTimeMicros / 1000000),
    
    ct:pal("Random access 1000 keys: ~.2f ms (~.0f ops/sec)", 
           [RandomTimeMs, RandomOpsPerSec]),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

%%====================================================================
%% Access Pattern Tests
%%====================================================================

test_sequential_vs_random_access(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Setup test data
    NumKeys = 10000,
    TestData = generate_test_data(NumKeys),
    
    % Insert data
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    % Sequential access
    Keys = [Key || {Key, _} <- TestData],
    
    {SeqTimeMicros, _} = measure_time(fun() ->
        [elmdb:get(DB, Key) || Key <- Keys]
    end),
    
    % Random access
    ShuffledKeys = [lists:nth(rand:uniform(NumKeys), Keys) || _ <- lists:seq(1, NumKeys)],
    
    {RandTimeMicros, _} = measure_time(fun() ->
        [elmdb:get(DB, Key) || Key <- ShuffledKeys]
    end),
    
    SeqOpsPerSec = NumKeys / (SeqTimeMicros / 1000000),
    RandOpsPerSec = NumKeys / (RandTimeMicros / 1000000),
    
    ct:pal("Sequential access: ~.0f ops/sec", [SeqOpsPerSec]),
    ct:pal("Random access: ~.0f ops/sec", [RandOpsPerSec]),
    ct:pal("Sequential/Random ratio: ~.2f", [SeqOpsPerSec / RandOpsPerSec]),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_readers(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Setup test data
    NumKeys = 10000,
    TestData = generate_test_data(NumKeys),
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    NumReaders = 4,
    ReadsPerReader = 2500,
    
    ct:pal("Testing ~p concurrent readers (~p reads each)", 
           [NumReaders, ReadsPerReader]),
    
    Keys = [Key || {Key, _} <- TestData],
    
    {TimeMicros, _} = measure_time(fun() ->
        Pids = [spawn_link(fun() ->
            lists:foreach(fun(_) ->
                RandomKey = lists:nth(rand:uniform(NumKeys), Keys),
                _ = elmdb:get(DB, RandomKey)
            end, lists:seq(1, ReadsPerReader))
        end) || _ <- lists:seq(1, NumReaders)],
        
        % Wait for all readers to complete
        lists:foreach(fun(Pid) ->
            receive
                {'EXIT', Pid, normal} -> ok;
                {'EXIT', Pid, Reason} -> error({reader_failed, Reason})
            after 30000 ->
                error(reader_timeout)
            end
        end, Pids)
    end),
    
    TotalReads = NumReaders * ReadsPerReader,
    TimeSeconds = TimeMicros / 1000000,
    OpsPerSecond = TotalReads / TimeSeconds,
    
    ct:pal("Concurrent reads: ~.2f seconds (~.0f ops/sec total)", 
           [TimeSeconds, OpsPerSecond]),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

test_concurrent_writers(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Note: This test may fail if the NIF doesn't support concurrent writes
    % LMDB typically requires serialized writes but allows concurrent reads
    
    NumWriters = 2,
    WritesPerWriter = 1000,
    
    ct:pal("Testing ~p concurrent writers (~p writes each)", 
           [NumWriters, WritesPerWriter]),
    
    {TimeMicros, Results} = measure_time(fun() ->
        Pids = [spawn_link(fun() ->
            WriterId = self(),
            try
                lists:foreach(fun(N) ->
                    Key = list_to_binary(io_lib:format("writer_~p_~w", [WriterId, N])),
                    Value = list_to_binary(io_lib:format("value_~p_~w", [WriterId, N])),
                    ok = elmdb:put(DB, Key, Value)
                end, lists:seq(1, WritesPerWriter)),
                exit(normal)
            catch
                Class:Reason:Stack ->
                    exit({Class, Reason, Stack})
            end
        end) || _ <- lists:seq(1, NumWriters)],
        
        % Wait for all writers to complete
        lists:map(fun(Pid) ->
            receive
                {'EXIT', Pid, normal} -> ok;
                {'EXIT', Pid, {error, Reason}} -> {error, Reason};
                {'EXIT', Pid, Reason} -> {error, Reason}
            after 30000 ->
                error(writer_timeout)
            end
        end, Pids)
    end),
    
    case lists:all(fun(R) -> R =:= ok end, Results) of
        true ->
            TotalWrites = NumWriters * WritesPerWriter,
            TimeSeconds = TimeMicros / 1000000,
            OpsPerSecond = TotalWrites / TimeSeconds,
            
            ct:pal("Concurrent writes: ~.2f seconds (~.0f ops/sec total)", 
                   [TimeSeconds, OpsPerSecond]);
        false ->
            ct:pal("Some concurrent writers failed: ~p", [Results]),
            ct:pal("This may be expected if the NIF serializes writes")
    end,
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

%%====================================================================
%% Memory and Growth Tests
%%====================================================================

test_memory_usage_patterns(Config) ->
    {Env, DB} = setup_database(Config, [{map_size, 52428800}]), % 50MB
    
    % Test growing database size
    BatchSizes = [1000, 2000, 3000, 4000, 5000],
    
    lists:foreach(fun(BatchSize) ->
        ct:pal("Adding batch of ~p records", [BatchSize]),
        
        BatchData = generate_test_data(BatchSize, 500), % 500 byte values
        
        {TimeMicros, _} = measure_time(fun() ->
            lists:foreach(fun({Key, Value}) ->
                ok = elmdb:put(DB, Key, Value)
            end, BatchData)
        end),
        
        TimeSeconds = TimeMicros / 1000000,
        OpsPerSecond = BatchSize / TimeSeconds,
        
        ct:pal("Batch ~p: ~.2f seconds (~.0f ops/sec)", 
               [BatchSize, TimeSeconds, OpsPerSecond])
        
    end, BatchSizes),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

test_list_performance(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Create hierarchical data for list testing
    NumGroups = 100,
    ItemsPerGroup = 50,
    
    ct:pal("Creating ~p groups with ~p items each for list testing", 
           [NumGroups, ItemsPerGroup]),
    
    % Create hierarchical data
    lists:foreach(fun(GroupNum) ->
        GroupPrefix = list_to_binary(io_lib:format("group_~3..0w", [GroupNum])),
        lists:foreach(fun(ItemNum) ->
            Key = <<GroupPrefix/binary, "/item_", (integer_to_binary(ItemNum))/binary>>,
            Value = <<"item_value">>,
            ok = elmdb:put(DB, Key, Value)
        end, lists:seq(1, ItemsPerGroup))
    end, lists:seq(1, NumGroups)),
    
    % Test list performance for different group sizes
    TestGroups = [1, 10, 50, 100],
    
    lists:foreach(fun(GroupNum) ->
        GroupPrefix = list_to_binary(io_lib:format("group_~3..0w", [GroupNum])),
        
        {TimeMicros, {ok, Items}} = measure_time(fun() ->
            elmdb:list(DB, GroupPrefix)
        end),
        
        TimeMs = TimeMicros / 1000,
        
        ct:pal("List group ~p (~p items): ~.2f ms", 
               [GroupNum, length(Items), TimeMs])
        
    end, TestGroups),
    
    % Test listing all items with common prefix
    {AllTimeMicros, {ok, AllItems}} = measure_time(fun() ->
        elmdb:list(DB, <<"group_">>)
    end),
    
    AllTimeMs = AllTimeMicros / 1000,
    
    ct:pal("List all groups (~p items): ~.2f ms", 
           [length(AllItems), AllTimeMs]),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

test_database_growth(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Test database growth patterns
    GrowthSteps = [1000, 5000, 10000, 20000],
    
    lists:foldl(fun(TotalSize, PrevSize) ->
        NewRecords = TotalSize - PrevSize,
        ct:pal("Growing database to ~p records (+~p)", [TotalSize, NewRecords]),
        
        NewData = [begin
                       Key = list_to_binary(io_lib:format("growth_~w", [N])),
                       Value = binary:copy(<<"G">>, 200), % 200 byte values
                       {Key, Value}
                   end || N <- lists:seq(PrevSize + 1, TotalSize)],
        
        {TimeMicros, _} = measure_time(fun() ->
            lists:foreach(fun({Key, Value}) ->
                ok = elmdb:put(DB, Key, Value)
            end, NewData)
        end),
        
        TimeSeconds = TimeMicros / 1000000,
        OpsPerSecond = NewRecords / TimeSeconds,
        
        ct:pal("Added ~p records: ~.2f seconds (~.0f ops/sec)", 
               [NewRecords, TimeSeconds, OpsPerSecond]),
        
        TotalSize
    end, 0, GrowthSteps),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.

test_key_iteration_performance(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Setup data for iteration testing
    NumKeys = 10000,
    TestData = generate_test_data(NumKeys, 100),
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    % Test different prefix lengths for iteration
    Prefixes = [<<"key_">>, <<"key_0">>, <<"key_00">>, <<"key_000">>],
    
    lists:foreach(fun(Prefix) ->
        {TimeMicros, Result} = measure_time(fun() ->
            elmdb:list(DB, Prefix)
        end),
        
        case Result of
            {ok, Keys} ->
                TimeMs = TimeMicros / 1000,
                KeysPerMs = length(Keys) / TimeMs,
                ct:pal("Prefix '~s': ~p keys in ~.2f ms (~.1f keys/ms)", 
                       [Prefix, length(Keys), TimeMs, KeysPerMs]);
            not_found ->
                ct:pal("Prefix '~s': no matches", [Prefix])
        end
    end, Prefixes),
    
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    ok.