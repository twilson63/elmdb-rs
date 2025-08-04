#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/elmdb/ebin

-mode(compile).

main(_) ->
    % Clean up and create test directory
    TestDir = "/tmp/elmdb_mixed_workload",
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
    
    io:format("~nElmDB Mixed Workload Performance Test~n"),
    io:format("======================================~n~n"),
    
    % Test 1: Many small writes followed by reads (message-like workload)
    io:format("Test 1: Message-like workload (many small writes + reads)~n"),
    
    % Simulate writing many messages
    WriteStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        MsgId = integer_to_binary(I),
        % Write message metadata
        elmdb:put(DB, <<"msg/", MsgId/binary, "/from">>, <<"user", (integer_to_binary(I rem 100))/binary>>),
        elmdb:put(DB, <<"msg/", MsgId/binary, "/to">>, <<"user", (integer_to_binary((I+50) rem 100))/binary>>),
        elmdb:put(DB, <<"msg/", MsgId/binary, "/subject">>, <<"Subject ", MsgId/binary>>),
        elmdb:put(DB, <<"msg/", MsgId/binary, "/body">>, <<"Message body for message ", MsgId/binary>>),
        elmdb:put(DB, <<"msg/", MsgId/binary, "/timestamp">>, integer_to_binary(erlang:system_time(second)))
    end, lists:seq(1, 1000)),
    WriteEnd = erlang:monotonic_time(microsecond),
    WriteTime = (WriteEnd - WriteStart) / 1000,
    
    io:format("  Wrote 5000 message fields in ~.2f ms (~.2f ops/ms)~n", 
              [WriteTime, 5000/WriteTime]),
    
    % Now do mixed reads and lists (typical message app pattern)
    MixedStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        MsgId = integer_to_binary(I),
        % Read message
        {ok, _From} = elmdb:get(DB, <<"msg/", MsgId/binary, "/from">>),
        {ok, _Subject} = elmdb:get(DB, <<"msg/", MsgId/binary, "/subject">>),
        % List message fields
        {ok, _Fields} = elmdb:list(DB, <<"msg/", MsgId/binary, "/">>)
    end, lists:seq(1, 100)),
    MixedEnd = erlang:monotonic_time(microsecond),
    MixedTime = (MixedEnd - MixedStart) / 1000,
    
    io:format("  Read 200 fields + 100 lists in ~.2f ms (~.2f ops/ms)~n", 
              [MixedTime, 300/MixedTime]),
    
    % Test 2: Interleaved writes and reads
    io:format("~nTest 2: Interleaved writes and reads~n"),
    
    InterleavedStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = <<"interleaved/", (integer_to_binary(I))/binary>>,
        Value = <<"value", (integer_to_binary(I))/binary>>,
        elmdb:put(DB, Key, Value),
        % Every 10 writes, do some reads
        case I rem 10 of
            0 ->
                lists:foreach(fun(J) ->
                    ReadKey = <<"interleaved/", (integer_to_binary(J))/binary>>,
                    elmdb:get(DB, ReadKey)
                end, lists:seq(I-9, I));
            _ ->
                ok
        end
    end, lists:seq(1, 1000)),
    InterleavedEnd = erlang:monotonic_time(microsecond),
    InterleavedTime = (InterleavedEnd - InterleavedStart) / 1000,
    
    io:format("  1000 writes + 1000 reads interleaved in ~.2f ms~n", [InterleavedTime]),
    
    % Test 3: Burst writes followed by burst reads
    io:format("~nTest 3: Burst pattern (write burst, then read burst)~n"),
    
    % Write burst
    BurstWriteStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = <<"burst/", (integer_to_binary(I))/binary>>,
        Value = <<"burst_value", (integer_to_binary(I))/binary>>,
        elmdb:put(DB, Key, Value)
    end, lists:seq(1, 5000)),
    BurstWriteEnd = erlang:monotonic_time(microsecond),
    BurstWriteTime = (BurstWriteEnd - BurstWriteStart) / 1000,
    
    io:format("  Write burst: 5000 writes in ~.2f ms (~.2f ops/ms)~n", 
              [BurstWriteTime, 5000/BurstWriteTime]),
    
    % Read burst
    BurstReadStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = <<"burst/", (integer_to_binary(I))/binary>>,
        {ok, _} = elmdb:get(DB, Key)
    end, lists:seq(1, 5000)),
    BurstReadEnd = erlang:monotonic_time(microsecond),
    BurstReadTime = (BurstReadEnd - BurstReadStart) / 1000,
    
    io:format("  Read burst: 5000 reads in ~.2f ms (~.2f ops/ms)~n", 
              [BurstReadTime, 5000/BurstReadTime]),
    
    % Summary
    io:format("~nSummary:~n"),
    TotalWrites = 5000 + 1000 + 5000,
    TotalReads = 200 + 1000 + 5000,
    TotalLists = 100,
    TotalOps = TotalWrites + TotalReads + TotalLists,
    TotalTime = WriteTime + MixedTime + InterleavedTime + BurstWriteTime + BurstReadTime,
    
    io:format("  Total operations: ~p~n", [TotalOps]),
    io:format("  Total time: ~.2f ms~n", [TotalTime]),
    io:format("  Average throughput: ~.2f ops/ms~n", [TotalOps/TotalTime]),
    
    elmdb:env_close(Env),
    io:format("~nTest completed.~n").