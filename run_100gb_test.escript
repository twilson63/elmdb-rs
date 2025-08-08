#!/usr/bin/env escript

main(_) ->
    % Add paths
    code:add_path("_build/default/lib/elmdb/ebin"),
    code:add_path("test"),
    
    % Load elmdb module
    case code:ensure_loaded(elmdb) of
        {module, elmdb} ->
            io:format("ElmDB module loaded successfully~n");
        LoadError ->
            io:format("Failed to load elmdb: ~p~n", [LoadError]),
            halt(1)
    end,
    
    io:format("~n=== Starting 100GB ElmDB Stress Test ===~n"),
    io:format("This test will write 100GB of random data to stress test the database~n~n"),
    
    Dir = "/tmp/elmdb_100gb_stress",
    file:del_dir_r(Dir),
    filelib:ensure_dir(Dir ++ "/"),
    
    GB = 1024 * 1024 * 1024,
    TargetSize = 100 * GB,
    
    % Open environment with 110GB map size
    {ok, Env} = elmdb:env_open(Dir, [
        {map_size, 110 * GB},
        no_sync,
        write_map
    ]),
    
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    io:format("Database opened successfully~n"),
    io:format("Starting write operations...~n~n"),
    
    % Track progress
    StartTime = erlang:monotonic_time(millisecond),
    
    % Write data in chunks
    ChunkSize = 100 * 1024 * 1024, % 100MB chunks
    NumChunks = TargetSize div ChunkSize,
    
    write_chunks(DB, NumChunks, ChunkSize, 0, StartTime),
    
    % Final stats
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000,
    
    io:format("~n=== Test Complete ===~n"),
    io:format("Total time: ~.2f seconds~n", [Duration]),
    io:format("Write throughput: ~.2f MB/s~n", [(TargetSize / (1024*1024)) / Duration]),
    
    % Verify some data
    io:format("~nVerifying random reads...~n"),
    verify_random_reads(DB, 1000),
    
    % Cleanup
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    
    io:format("~nCleaning up test directory...~n"),
    file:del_dir_r(Dir),
    
    io:format("Test completed successfully!~n"),
    halt(0).

write_chunks(_DB, 0, _ChunkSize, TotalWritten, StartTime) ->
    io:format("~nAll chunks written. Total: ~.2f GB~n", [TotalWritten / (1024*1024*1024)]);
write_chunks(DB, RemainingChunks, ChunkSize, TotalWritten, StartTime) ->
    % Write one chunk
    BytesWritten = write_chunk(DB, ChunkSize),
    NewTotal = TotalWritten + BytesWritten,
    
    % Report progress
    CurrentTime = erlang:monotonic_time(millisecond),
    ElapsedSec = (CurrentTime - StartTime) / 1000,
    CurrentGB = NewTotal / (1024*1024*1024),
    Throughput = (NewTotal / (1024*1024)) / max(1, ElapsedSec),
    
    io:format("Progress: ~.2f GB written (~.1f%%) - ~.2f MB/s~n", 
              [CurrentGB, (NewTotal * 100) / (100 * 1024*1024*1024), Throughput]),
    
    write_chunks(DB, RemainingChunks - 1, ChunkSize, NewTotal, StartTime).

write_chunk(DB, TargetBytes) ->
    write_chunk_loop(DB, TargetBytes, 0).

write_chunk_loop(_DB, TargetBytes, Written) when Written >= TargetBytes ->
    Written;
write_chunk_loop(DB, TargetBytes, Written) ->
    % Generate random key and value
    KeySize = 32 + rand:uniform(224), % 32-256 bytes
    ValueSize = 1024 + rand:uniform(9 * 1024), % 1KB-10KB
    
    Key = crypto:strong_rand_bytes(KeySize),
    Value = crypto:strong_rand_bytes(ValueSize),
    
    case elmdb:put(DB, Key, Value) of
        ok ->
            write_chunk_loop(DB, TargetBytes, Written + KeySize + ValueSize);
        {error, _, Msg} ->
            io:format("Write error: ~s~n", [Msg]),
            write_chunk_loop(DB, TargetBytes, Written)
    end.

verify_random_reads(DB, Count) ->
    verify_reads_loop(DB, Count, 0, 0).

verify_reads_loop(_DB, 0, Success, Failures) ->
    io:format("Verification complete: ~p successful, ~p not found~n", [Success, Failures]);
verify_reads_loop(DB, Remaining, Success, Failures) ->
    % Generate random key
    Key = crypto:strong_rand_bytes(32 + rand:uniform(224)),
    
    case elmdb:get(DB, Key) of
        {ok, _Value} ->
            verify_reads_loop(DB, Remaining - 1, Success + 1, Failures);
        not_found ->
            verify_reads_loop(DB, Remaining - 1, Success, Failures + 1);
        Error ->
            io:format("Read error: ~p~n", [Error]),
            verify_reads_loop(DB, Remaining - 1, Success, Failures + 1)
    end.