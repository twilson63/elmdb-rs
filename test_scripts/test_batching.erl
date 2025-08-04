#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/elmdb/ebin

%% Simple test to verify transaction batching optimization works
main(_) ->
    TestDir = "/tmp/elmdb_batch_test",
    os:cmd("rm -rf " ++ TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/"),

    %% Open environment and database
    {ok, Env} = elmdb:env_open(TestDir, [{map_size, 1048576}]),
    {ok, DB} = elmdb:db_open(Env, [create]),

    %% Test many small writes (should be batched automatically)
    io:format("Testing transaction batching with 500 small writes...~n"),
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(I) ->
        Key = <<"key_", (integer_to_binary(I))/binary>>,
        Value = <<"value_", (integer_to_binary(I))/binary>>,
        ok = elmdb:put(DB, Key, Value)
    end, lists:seq(1, 500)),
    
    %% Force flush to commit any remaining buffered writes
    ok = elmdb:flush(DB),
    
    EndTime = erlang:monotonic_time(microsecond),
    WriteTime = (EndTime - StartTime) / 1000,
    
    io:format("500 writes completed in ~.2f ms~n", [WriteTime]),

    %% Verify all data was written correctly
    io:format("Verifying all data was written...~n"),
    ReadStartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(I) ->
        Key = <<"key_", (integer_to_binary(I))/binary>>,
        ExpectedValue = <<"value_", (integer_to_binary(I))/binary>>,
        {ok, ActualValue} = elmdb:get(DB, Key),
        case ActualValue of
            ExpectedValue -> ok;
            _ -> 
                io:format("ERROR: Key ~p expected ~p but got ~p~n", 
                         [Key, ExpectedValue, ActualValue]),
                halt(1)
        end
    end, lists:seq(1, 500)),
    
    ReadEndTime = erlang:monotonic_time(microsecond),
    ReadTime = (ReadEndTime - ReadStartTime) / 1000,
    
    io:format("500 reads completed in ~.2f ms~n", [ReadTime]),
    io:format("All data verified correctly!~n"),

    %% Test mixed read/write pattern to verify flush behavior
    io:format("Testing mixed read/write pattern...~n"),
    MixedStartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(I) ->
        Key = <<"mixed_", (integer_to_binary(I))/binary>>,
        Value = <<"mixed_value_", (integer_to_binary(I))/binary>>,
        ok = elmdb:put(DB, Key, Value),
        
        %% Every 10th operation, do a read to trigger flush
        case I rem 10 of
            0 -> 
                {ok, _} = elmdb:get(DB, Key);
            _ -> 
                ok
        end
    end, lists:seq(1, 100)),
    
    MixedEndTime = erlang:monotonic_time(microsecond),
    MixedTime = (MixedEndTime - MixedStartTime) / 1000,
    
    io:format("Mixed read/write pattern completed in ~.2f ms~n", [MixedTime]),

    %% Cleanup
    elmdb:env_close(Env),
    os:cmd("rm -rf " ++ TestDir),
    
    io:format("Transaction batching test completed successfully!~n"),
    io:format("Performance summary:~n"),
    io:format("  - Batched writes: ~.2f ms for 500 operations (~.3f ms/op)~n", 
              [WriteTime, WriteTime/500]),
    io:format("  - Reads: ~.2f ms for 500 operations (~.3f ms/op)~n", 
              [ReadTime, ReadTime/500]),
    io:format("  - Mixed operations: ~.2f ms for 100 operations (~.3f ms/op)~n", 
              [MixedTime, MixedTime/100]).