#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/elmdb/ebin +K true +A 10

-mode(compile).

main(Args) ->
    % Parse arguments for size
    SizeGB = case Args of
        [Size] -> list_to_integer(Size);
        _ -> 1  % Default to 1GB for testing
    end,
    
    TestDir = "/tmp/elmdb_large_scale",
    os:cmd("rm -rf " ++ TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    % Calculate map size - need extra space for overhead
    MapSize = (SizeGB + 1) * 1024 * 1024 * 1024,
    
    io:format("~nElmDB Large Scale Performance Test (~p GB)~n", [SizeGB]),
    io:format("==========================================~n~n"),
    
    % Open environment with large map size
    {ok, Env} = elmdb:env_open(TestDir, [
        {map_size, MapSize},
        no_sync,
        no_mem_init,
        write_map
    ]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Generate test data to reach target size
    % Assuming average key size of 32 bytes and value size of 1KB
    KeySize = 32,
    ValueSize = 1024,
    RecordSize = KeySize + ValueSize,
    RecordsNeeded = (SizeGB * 1024 * 1024 * 1024) div RecordSize,
    
    io:format("Populating database with ~p records (~p MB each batch)...~n", 
              [RecordsNeeded, (RecordsNeeded div 100) * RecordSize div (1024*1024)]),
    
    % Write data in batches
    BatchSize = RecordsNeeded div 100,
    Value = list_to_binary(lists:duplicate(ValueSize, $x)),
    
    WriteStart = erlang:monotonic_time(millisecond),
    lists:foreach(fun(Batch) ->
        StartIdx = Batch * BatchSize,
        EndIdx = min((Batch + 1) * BatchSize, RecordsNeeded),
        
        lists:foreach(fun(I) ->
            Key = io_lib:format("key~12..0B", [I]),
            elmdb:put(DB, iolist_to_binary(Key), Value)
        end, lists:seq(StartIdx, EndIdx - 1)),
        
        if Batch rem 10 == 0 ->
            Progress = (Batch + 1),
            io:format("  Progress: ~p% (~p MB written)~n", 
                     [Progress, (EndIdx * RecordSize) div (1024*1024)]);
        true -> ok
        end
    end, lists:seq(0, 99)),
    WriteEnd = erlang:monotonic_time(millisecond),
    WriteTime = WriteEnd - WriteStart,
    
    ActualSizeMB = (RecordsNeeded * RecordSize) div (1024*1024),
    io:format("~nDatabase populated: ~p MB in ~.2f seconds (~.2f MB/s)~n",
              [ActualSizeMB, WriteTime/1000, ActualSizeMB/(WriteTime/1000)]),
    
    % Test read performance at scale
    io:format("~nTesting read performance at ~p GB scale...~n", [SizeGB]),
    
    % Random reads
    RandomKeys = [io_lib:format("key~12..0B", [rand:uniform(RecordsNeeded)]) 
                  || _ <- lists:seq(1, 1000)],
    
    RandomReadStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(Key) ->
        {ok, _} = elmdb:get(DB, iolist_to_binary(Key))
    end, RandomKeys),
    RandomReadEnd = erlang:monotonic_time(microsecond),
    RandomReadTime = (RandomReadEnd - RandomReadStart) / 1000,
    
    io:format("  Random reads: 1000 reads in ~.2f ms (~.2f μs/read)~n",
              [RandomReadTime, RandomReadTime]),
    
    % Sequential reads
    SeqReadStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = io_lib:format("key~12..0B", [I]),
        {ok, _} = elmdb:get(DB, iolist_to_binary(Key))
    end, lists:seq(1, 1000)),
    SeqReadEnd = erlang:monotonic_time(microsecond),
    SeqReadTime = (SeqReadEnd - SeqReadStart) / 1000,
    
    io:format("  Sequential reads: 1000 reads in ~.2f ms (~.2f μs/read)~n",
              [SeqReadTime, SeqReadTime]),
    
    % Test write performance at scale
    io:format("~nTesting write performance at ~p GB scale...~n", [SizeGB]),
    
    NewWriteStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = io_lib:format("newkey~12..0B", [I]),
        elmdb:put(DB, iolist_to_binary(Key), Value)
    end, lists:seq(1, 1000)),
    NewWriteEnd = erlang:monotonic_time(microsecond),
    NewWriteTime = (NewWriteEnd - NewWriteStart) / 1000,
    
    io:format("  New writes: 1000 writes in ~.2f ms (~.2f μs/write)~n",
              [NewWriteTime, NewWriteTime]),
    
    % Test list performance at scale
    io:format("~nTesting list performance at ~p GB scale...~n", [SizeGB]),
    
    ListStart = erlang:monotonic_time(microsecond),
    {ok, Children} = elmdb:list(DB, <<"key">>),
    ListEnd = erlang:monotonic_time(microsecond),
    ListTime = (ListEnd - ListStart) / 1000,
    
    io:format("  List with prefix 'key': ~p results in ~.2f ms~n",
              [length(Children), ListTime]),
    
    % Summary
    io:format("~nPerformance Summary at ~p GB:~n", [SizeGB]),
    io:format("  Random read latency: ~.2f μs~n", [RandomReadTime]),
    io:format("  Sequential read latency: ~.2f μs~n", [SeqReadTime]),
    io:format("  Write latency: ~.2f μs~n", [NewWriteTime]),
    io:format("  List operation: ~.2f ms for ~p items~n", [ListTime, length(Children)]),
    
    % Memory stats
    {ok, MemInfo} = file:read_file("/proc/meminfo"),
    Lines = binary:split(MemInfo, <<"\n">>, [global]),
    CachedLine = lists:filter(fun(Line) ->
        case binary:match(Line, <<"Cached:">>) of
            nomatch -> false;
            _ -> true
        end
    end, Lines),
    case CachedLine of
        [Cache|_] ->
            io:format("  OS Page Cache: ~s~n", [Cache]);
        _ -> ok
    end,
    
    elmdb:env_close(Env),
    io:format("~nTest completed.~n").