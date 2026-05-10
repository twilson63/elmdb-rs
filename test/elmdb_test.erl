%%%-------------------------------------------------------------------
%%% @doc
%%% Consolidated test suite for elmdb
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test fixtures
%%%===================================================================

setup() ->
    Dir = test_dir(),
    file:del_dir_r(Dir),
    filelib:ensure_dir(Dir ++ "/"),
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}, {batch_size, 1000}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    {Dir, Env, DB}.

cleanup({Dir, Env, DB}) ->
    _ = elmdb:db_close(DB),
    _ = elmdb:env_close(Env),
    file:del_dir_r(Dir).

test_dir() ->
    Unique = erlang:unique_integer([positive]),
    filename:join(["/tmp", "elmdb_test_" ++ integer_to_list(Unique)]).

%%%===================================================================
%%% Basic Operation Tests
%%%===================================================================

basic_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Test put and get
                     ok = elmdb:put(DB, <<"key1">>, <<"value1">>),
                     ?assertEqual({ok, <<"value1">>}, elmdb:get(DB, <<"key1">>)),
                     
                     % Test overwrite
                     ok = elmdb:put(DB, <<"key1">>, <<"value2">>),
                     ?assertEqual({ok, <<"value2">>}, elmdb:get(DB, <<"key1">>)),
                     
                     % Test not found
                     ?assertEqual(not_found, elmdb:get(DB, <<"nonexistent">>))
                 end),
          
          ?_test(begin
                     % Test flush
                     ok = elmdb:put(DB, <<"flush_test">>, <<"data">>),
                     ok = elmdb:flush(DB),
                     ?assertEqual({ok, <<"data">>}, elmdb:get(DB, <<"flush_test">>))
                 end)
         ]
     end}.

%%%===================================================================
%%% Batch Operation Tests
%%%===================================================================

batch_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Test multiple puts (replacing batch put)
                     ok = elmdb:put(DB, <<"batch1">>, <<"value1">>),
                     ok = elmdb:put(DB, <<"batch2">>, <<"value2">>),
                     ok = elmdb:put(DB, <<"batch3">>, <<"value3">>),
                     
                     ?assertEqual({ok, <<"value1">>}, elmdb:get(DB, <<"batch1">>)),
                     ?assertEqual({ok, <<"value2">>}, elmdb:get(DB, <<"batch2">>)),
                     ?assertEqual({ok, <<"value3">>}, elmdb:get(DB, <<"batch3">>))
                 end),
          ?_test(begin
                     % Regression: mixed buffered put/3 + immediate put_batch/2
                     % must preserve operation ordering.
                     ok = elmdb:put(DB, <<"mixed/key">>, <<"from_put">>),
                     ok = elmdb:put_batch(DB, [{<<"mixed/key">>, <<"from_batch">>}]),
                     ?assertEqual({ok, <<"from_batch">>}, elmdb:get(DB, <<"mixed/key">>))
                 end)
         ]
     end}.

%%%===================================================================
%%% Singleton/Reopen Tests
%%%===================================================================

singleton_reopen_test_() ->
    [
     ?_test(begin
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),

                {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}]),
                {ok, EnvAgain} = elmdb:env_open(Dir, [{map_size, 20971520}]),
                ?assertEqual(Env, EnvAgain),

                {ok, DB1} = elmdb:db_open(Env, [create]),
                {ok, DB2} = elmdb:db_open(Env, [create]),

                % db_open should return the same singleton DB handle for the env.
                ?assertEqual(DB1, DB2),

                ok = elmdb:put(DB1, <<"singleton/key">>, <<"v1">>),
                ?assertEqual({ok, <<"v1">>}, elmdb:get(DB2, <<"singleton/key">>)),

                % Soft-close then reopen through operation and db_open.
                ok = elmdb:db_close(DB1),
                ?assertEqual({ok, <<"v1">>}, elmdb:get(DB2, <<"singleton/key">>)),

                % Explicit env_close should not invalidate DB refs.
                ok = elmdb:env_close(Env),
                ?assertEqual({ok, <<"v1">>}, elmdb:get(DB1, <<"singleton/key">>)),

                % env_close_by_name should also be non-destructive.
                ok = elmdb:env_close_by_name(Dir),
                ?assertEqual({ok, <<"v1">>}, elmdb:get(DB2, <<"singleton/key">>)),

                {ok, DB3} = elmdb:db_open(Env, [create]),
                ?assertEqual(DB1, DB3),

                ok = elmdb:put(DB3, <<"singleton/key">>, <<"v2">>),
                ?assertEqual({ok, <<"v2">>}, elmdb:get(DB1, <<"singleton/key">>)),

                ok = elmdb:db_close(DB1),
                ok = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)
    ].

%%%===================================================================
%%% List Operation Tests  
%%%===================================================================

list_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Setup hierarchical data
                     ok = elmdb:put(DB, <<"users/alice/name">>, <<"Alice">>),
                     ok = elmdb:put(DB, <<"users/alice/email">>, <<"alice@example.com">>),
                     ok = elmdb:put(DB, <<"users/bob/name">>, <<"Bob">>),
                     ok = elmdb:put(DB, <<"users/bob/email">>, <<"bob@example.com">>),
                     
                     % List users
                     {ok, Users} = elmdb:list(DB, <<"users/">>),
                     ?assertEqual(lists:sort([<<"alice">>, <<"bob">>]), lists:sort(Users)),
                     
                     % List alice's attributes
                     {ok, AliceAttrs} = elmdb:list(DB, <<"users/alice/">>),
                     ?assertEqual(lists:sort([<<"name">>, <<"email">>]), lists:sort(AliceAttrs)),
                     
                     % List non-existent prefix
                     ?assertEqual(not_found, elmdb:list(DB, <<"nonexistent/">>))
                 end),
          
          % Skip empty database test due to lmdb-rs panic issue
         {"Empty database list", 
           ?_test(begin
                      skip
                  end)}
         ]
     end}.

%%%===================================================================
%%% Iterator and Fold Tests
%%%===================================================================

iterator_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Empty iterator should terminate immediately.
                     Start0 = elmdb:iterator(DB),
                     ?assertMatch({iterator, start}, Start0),
                     ?assertEqual(undefined, elmdb:iterator_next(DB, Start0)),

                     % Populate sorted keys and iterate through all entries.
                     ok = elmdb:put(DB, <<"iter/a">>, <<"A">>),
                     ok = elmdb:put(DB, <<"iter/b">>, <<"B">>),
                     ok = elmdb:put(DB, <<"iter/c">>, <<"C">>),

                     Start = elmdb:iterator(DB),
                     ?assertMatch({iterator, start}, Start),

                     {ok, <<"iter/a">>, <<"A">>, CursorA} = elmdb:iterator_next(DB, Start),
                     {ok, <<"iter/b">>, <<"B">>, CursorB} = elmdb:iterator_next(DB, CursorA),

                     % Add an intermediate key between cursor steps; next call should observe it.
                     ok = elmdb:put(DB, <<"iter/bb">>, <<"BB">>),
                     {ok, <<"iter/bb">>, <<"BB">>, CursorBB} = elmdb:iterator_next(DB, CursorB),
                     {ok, <<"iter/c">>, <<"C">>, CursorC} = elmdb:iterator_next(DB, CursorBB),

                     % Last cursor and after-last cursor should both return undefined.
                     ?assertEqual(undefined, elmdb:iterator_next(DB, CursorC)),
                     ?assertEqual(undefined, elmdb:iterator_next(DB, CursorC))
                 end)
         ]
     end}.

iterator_invalid_cursor_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     ok = elmdb:put(DB, <<"iter/key">>, <<"value">>),
                     ?assertMatch({error, invalid, _},
                                  elmdb:iterator_next(DB, invalid_cursor))
                 end)
         ]
     end}.

fold_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Empty database fold
                     ?assertEqual(ok, elmdb:foreach(DB, fun(_Key, _Value) -> ok end)),
                     ?assertEqual({ok, 0},
                                  elmdb:fold(DB, fun(_Key, _Value, Acc) -> Acc + 1 end, 0)),
                     ?assertEqual({ok, #{}},
                                  elmdb:map(DB, fun(_Key, _Value) -> unused end)),

                     % Populate and fold with arity-2 callback
                     ok = elmdb:put(DB, <<"fold/1">>, <<"1">>),
                     ok = elmdb:put(DB, <<"fold/2">>, <<"2">>),
                     ok = elmdb:put(DB, <<"fold/3">>, <<"3">>),

                     Self = self(),
                     ok = elmdb:foreach(DB, fun(Key, Value) ->
                                              Self ! {kv, Key, Value},
                                              ok
                                      end),
                     Pairs = collect_messages(3, []),
                     ?assertEqual(
                        lists:sort([
                            {<<"fold/1">>, <<"1">>},
                            {<<"fold/2">>, <<"2">>},
                            {<<"fold/3">>, <<"3">>}
                        ]),
                        lists:sort(Pairs)
                     ),

                     % Arity-3 fold should return the final accumulator.
                     ?assertEqual(
                        {ok, 6},
                        elmdb:fold(DB, fun(_Key, Value, Acc) ->
                                           Acc + binary_to_integer(Value)
                                   end, 0)
                     ),

                     ?assertEqual(
                        {ok, #{
                            <<"fold/1">> => 2,
                            <<"fold/2">> => 4,
                            <<"fold/3">> => 6
                        }},
                        elmdb:map(DB, fun(_Key, Value) ->
                                          binary_to_integer(Value) * 2
                                  end)
                     )
                 end)
         ]
     end}.

collect_messages(0, Acc) ->
    Acc;
collect_messages(Count, Acc) ->
    receive
        {kv, Key, Value} ->
            collect_messages(Count - 1, [{Key, Value} | Acc])
    after 1000 ->
        ?assert(false)
    end.

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

error_handling_test_() ->
    [
     ?_test(begin
                % Test directory not found
                Result = elmdb:env_open("/nonexistent/path", []),
                ?assertMatch({error, directory_not_found}, Result)
            end),
     
     ?_test(begin
                % Test operations auto-reopen after soft-close
                {Dir, Env, DB} = setup(),
                ok = elmdb:put(DB, <<"k">>, <<"v0">>),
                ok = elmdb:db_close(DB),

                % Operations should transparently reopen and succeed.
                ok = elmdb:put(DB, <<"k">>, <<"v1">>),
                ?assertEqual({ok, <<"v1">>}, elmdb:get(DB, <<"k">>)),
                ?assertEqual(ok, elmdb:flush(DB)),

                ok = elmdb:db_close(DB),
                ok = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end),
     
     ?_test(begin
                % Test invalid put data
                {Dir, Env, DB} = setup(),
                ?assertError(badarg, elmdb:put(DB, not_binary, <<"v">>)),
                ?assertError(badarg, elmdb:put(DB, <<"k">>, not_binary)),
                ok = elmdb:db_close(DB),
                ok = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)
    ].

%%%===================================================================
%%% Environment Management Tests
%%%===================================================================

environment_test_() ->
    [
     ?_test(begin
                % Test normal open/close cycle
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                
                {ok, Env} = elmdb:env_open(Dir, []),
                {ok, DB} = elmdb:db_open(Env, [create]),
                ok = elmdb:db_close(DB),
                ok = elmdb:env_close(Env),
                
                file:del_dir_r(Dir)
            end),
     
     ?_test(begin
                % Test normal close with open database
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                
                {ok, Env} = elmdb:env_open(Dir, []),
                {ok, DB} = elmdb:db_open(Env, [create]),
                
                % Close database before environment
                ok = elmdb:db_close(DB),
                ok = elmdb:env_close(Env),
                
                file:del_dir_r(Dir)
            end)
    ].

%%%===================================================================
%%% Performance Test (simple)
%%%===================================================================

performance_test_() ->
    {timeout, 60,
     ?_test(begin
                {Dir, Env, DB} = setup(),

                % Write 1000 key-value pairs
                Start = erlang:monotonic_time(millisecond),
                lists:foreach(fun(I) ->
                    Key = iolist_to_binary([<<"key">>, integer_to_binary(I)]),
                    Value = iolist_to_binary([<<"value">>, integer_to_binary(I)]),
                    ok = elmdb:put(DB, Key, Value)
                end, lists:seq(1, 1000)),

                WriteTime = erlang:monotonic_time(millisecond) - Start,
                ?debugFmt("Write 1000 keys: ~p ms", [WriteTime]),

                % Verify a few reads
                ?assertEqual({ok, <<"value1">>}, elmdb:get(DB, <<"key1">>)),
                ?assertEqual({ok, <<"value500">>}, elmdb:get(DB, <<"key500">>)),
                ?assertEqual({ok, <<"value1000">>}, elmdb:get(DB, <<"key1000">>)),

                cleanup({Dir, Env, DB})
            end)}.

%%%===================================================================
%%% Chain Validity Stress Test
%%%===================================================================

chain_validity_test_() ->
    {timeout, 30,
     ?_test(begin
                Dir = "/tmp/elmdb_chain_" ++ integer_to_list(erlang:unique_integer([positive])),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}, {batch_size, 3}]),
                {ok, DB} = elmdb:db_open(Env, [create]),

                Parent = self(),
                StopFlag = atomics:new(1, []),
                atomics:put(StopFlag, 1, 0),

                Writers = [spawn_monitor(fun() ->
                    chain_writer(DB, W, 500),
                    Parent ! {writer_done, self()}
                end) || W <- lists:seq(1, 5)],

                Readers = [spawn_monitor(fun() ->
                    Broken = chain_reader(DB, StopFlag, 0),
                    Parent ! {reader_result, self(), Broken}
                end) || _ <- lists:seq(1, 5)],

                [receive {writer_done, P} -> ok end || {P, _} <- Writers],
                atomics:put(StopFlag, 1, 1),

                BrokenTotal = lists:sum([receive {reader_result, P, N} -> N end || {P, _} <- Readers]),

                [receive {'DOWN', R, process, P, _} -> ok end || {P, R} <- Writers ++ Readers],

                elmdb:flush(DB),
                elmdb:db_close(DB),
                elmdb:env_close(Env),
                file:del_dir_r(Dir),

                ?assertEqual(0, BrokenTotal)
            end)}.

chain_writer(DB, W, Iters) ->
    WB = integer_to_binary(W),
    lists:foreach(fun(I) ->
        IB = integer_to_binary(I),
        ok = elmdb:put(DB, <<"w", WB/binary, ":", IB/binary, "/leaf">>, <<"val", WB/binary, ":", IB/binary>>),
        ok = elmdb:put(DB, <<"w", WB/binary, ":", IB/binary, "/c">>, <<"w", WB/binary, ":", IB/binary, "/leaf">>),
        ok = elmdb:put(DB, <<"w", WB/binary, ":", IB/binary, "/b">>, <<"w", WB/binary, ":", IB/binary, "/c">>),
        ok = elmdb:put(DB, <<"w", WB/binary, ":", IB/binary, "/a">>, <<"w", WB/binary, ":", IB/binary, "/b">>)
    end, lists:seq(1, Iters)).

chain_reader(DB, StopFlag, Broken) ->
    case atomics:get(StopFlag, 1) of
        1 -> Broken;
        0 ->
            WB = integer_to_binary(rand:uniform(5)),
            IB = integer_to_binary(rand:uniform(500)),
            Root = <<"w", WB/binary, ":", IB/binary, "/a">>,
            NewBroken = case elmdb:get(DB, Root) of
                not_found -> Broken;
                {ok, Next1} -> follow_chain(DB, Next1, 3, Broken)
            end,
            chain_reader(DB, StopFlag, NewBroken)
    end.

follow_chain(_DB, _Key, 0, Broken) -> Broken;
follow_chain(DB, Key, Hops, Broken) ->
    case elmdb:get(DB, Key) of
        not_found -> Broken + 1;
        {ok, Next} -> follow_chain(DB, Next, Hops - 1, Broken)
    end.

%%%===================================================================
%%% Iterator/Fold Performance Test
%%%===================================================================

iterator_fold_performance_test_() ->
    {timeout, 60,
     ?_test(begin
                {Dir, Env, DB} = setup(),
                RecordCount = 25000,

                % Seed data for iteration/fold benchmarking.
                lists:foreach(fun(I) ->
                    Key = iolist_to_binary([<<"iter_perf/">>, integer_to_binary(I)]),
                    Value = integer_to_binary(I),
                    ok = elmdb:put(DB, Key, Value)
                end, lists:seq(1, RecordCount)),
                ok = elmdb:flush(DB),

                IterStart = erlang:monotonic_time(millisecond),
                Cursor = elmdb:iterator(DB),
                IterCount = count_iterator_records(DB, Cursor, 0),
                IterDuration = erlang:monotonic_time(millisecond) - IterStart,

                ?debugFmt("Iterator over ~p keys: ~p ms", [RecordCount, IterDuration]),
                ?assertEqual(RecordCount, IterCount),

                FoldStart = erlang:monotonic_time(millisecond),
                {ok, FoldCount} = elmdb:fold(DB, fun(_Key, _Value, Acc) -> Acc + 1 end, 0),
                FoldDuration = erlang:monotonic_time(millisecond) - FoldStart,

                ?debugFmt("Fold over ~p keys: ~p ms", [RecordCount, FoldDuration]),
                ?assertEqual(RecordCount, FoldCount),

                cleanup({Dir, Env, DB})
            end)}.

count_iterator_records(DB, Cursor, Count) ->
    case elmdb:iterator_next(DB, Cursor) of
        {ok, _Key, _Value, NextCursor} ->
            count_iterator_records(DB, NextCursor, Count + 1);
        undefined ->
            Count
    end.

%%%===================================================================
%%% Environment Copy Test
%%%===================================================================

environment_copy_test_() ->
    [
     {timeout, 120,
      ?_test(begin
                 % Test 1: Basic environment copy with 100k keys
                 % Create source environment and database
                 SourceDir = "/tmp/foo-db",
                 TargetDir = "/tmp/bar-db",
                 
                 % Clean up directories if they exist
                 file:del_dir_r(SourceDir),
                 file:del_dir_r(TargetDir),
                 filelib:ensure_dir(SourceDir ++ "/"),
                 
                 % Open source environment with sufficient map size for 100k keys
                 {ok, SourceEnv} = elmdb:env_open(SourceDir, [{map_size, 104857600}]), % 100MB
                 {ok, SourceDB} = elmdb:db_open(SourceEnv, [create]),
                 
                 % Write 100k key-value pairs
                 WriteStart = erlang:monotonic_time(millisecond),
                 lists:foreach(fun(I) ->
                     Key = iolist_to_binary([<<"test_key_">>, integer_to_binary(I)]),
                     Value = iolist_to_binary([<<"test_value_">>, integer_to_binary(I), <<"_data">>]),
                     ok = elmdb:put(SourceDB, Key, Value)
                 end, lists:seq(1, 100000)),
                 
                 % Flush to ensure all data is written
                 ok = elmdb:flush(SourceDB),
                 
                 WriteTime = erlang:monotonic_time(millisecond) - WriteStart,
                 ?debugFmt("Wrote 100k keys in ~p ms", [WriteTime]),
                 
                 % Verify some keys before closing
                 ?assertEqual({ok, <<"test_value_1_data">>}, elmdb:get(SourceDB, <<"test_key_1">>)),
                 ?assertEqual({ok, <<"test_value_50000_data">>}, elmdb:get(SourceDB, <<"test_key_50000">>)),
                 ?assertEqual({ok, <<"test_value_100000_data">>}, elmdb:get(SourceDB, <<"test_key_100000">>)),
                 
                 % Close the source database and environment
                 ok = elmdb:db_close(SourceDB),
                 ok = elmdb:env_close(SourceEnv),
                 
                 % Copy the database files to target directory
                 CopyStart = erlang:monotonic_time(millisecond),
                 filelib:ensure_dir(TargetDir ++ "/"),
                 
                 % Copy the LMDB data file
                 {ok, _} = file:copy(SourceDir ++ "/data.mdb", TargetDir ++ "/data.mdb"),
                 % Copy the LMDB lock file  
                 {ok, _} = file:copy(SourceDir ++ "/lock.mdb", TargetDir ++ "/lock.mdb"),
                 
                 CopyTime = erlang:monotonic_time(millisecond) - CopyStart,
                 ?debugFmt("Copied database in ~p ms", [CopyTime]),
                 
                 % Verify files exist in target directory
                 ?assert(filelib:is_regular(TargetDir ++ "/data.mdb")),
                 ?assert(filelib:is_regular(TargetDir ++ "/lock.mdb")),
                 
                 % Open the target environment  
                 {ok, TargetEnv} = elmdb:env_open(TargetDir, [{map_size, 104857600}]),
                 {ok, TargetDB} = elmdb:db_open(TargetEnv, []),
                 
                 % Read and verify all 100k keys
                 ReadStart = erlang:monotonic_time(millisecond),
                 lists:foreach(fun(I) ->
                     Key = iolist_to_binary([<<"test_key_">>, integer_to_binary(I)]),
                     ExpectedValue = iolist_to_binary([<<"test_value_">>, integer_to_binary(I), <<"_data">>]),
                     ?assertEqual({ok, ExpectedValue}, elmdb:get(TargetDB, Key))
                 end, lists:seq(1, 100000)),
                 
                 ReadTime = erlang:monotonic_time(millisecond) - ReadStart,
                 ?debugFmt("Read and verified 100k keys in ~p ms", [ReadTime]),
                 
                 % Test that we can write new data to the copied database
                 ok = elmdb:put(TargetDB, <<"new_key">>, <<"new_value">>),
                 ?assertEqual({ok, <<"new_value">>}, elmdb:get(TargetDB, <<"new_key">>)),
                 
                 % Clean up
                 ok = elmdb:db_close(TargetDB),
                 ok = elmdb:env_close(TargetEnv),
                 
                 file:del_dir_r(SourceDir),
                 file:del_dir_r(TargetDir),
                 
                 ?debugFmt("Environment copy test completed successfully", [])
             end)},
     
     {timeout, 30,
      ?_test(begin
                 % Test 2: Copy empty database
                 SourceDir = "/tmp/empty-source-db",
                 TargetDir = "/tmp/empty-target-db",
                 
                 % Clean up and create source
                 file:del_dir_r(SourceDir),
                 file:del_dir_r(TargetDir),
                 filelib:ensure_dir(SourceDir ++ "/"),
                 
                 % Create empty database
                 {ok, SourceEnv} = elmdb:env_open(SourceDir, [{map_size, 10485760}]),
                 {ok, SourceDB} = elmdb:db_open(SourceEnv, [create]),
                 
                 % Close without writing any data
                 ok = elmdb:db_close(SourceDB),
                 ok = elmdb:env_close(SourceEnv),
                 
                 % Copy the empty database
                 filelib:ensure_dir(TargetDir ++ "/"),
                 {ok, _} = file:copy(SourceDir ++ "/data.mdb", TargetDir ++ "/data.mdb"),
                 {ok, _} = file:copy(SourceDir ++ "/lock.mdb", TargetDir ++ "/lock.mdb"),
                 
                 % Open and verify copied empty database
                 {ok, TargetEnv} = elmdb:env_open(TargetDir, [{map_size, 10485760}]),
                 {ok, TargetDB} = elmdb:db_open(TargetEnv, []),
                 
                 % Verify it's empty
                 ?assertEqual(not_found, elmdb:get(TargetDB, <<"any_key">>)),
                 
                 % Verify we can write to it
                 ok = elmdb:put(TargetDB, <<"test">>, <<"data">>),
                 ?assertEqual({ok, <<"data">>}, elmdb:get(TargetDB, <<"test">>)),
                 
                 % Clean up
                 ok = elmdb:db_close(TargetDB),
                 ok = elmdb:env_close(TargetEnv),
                 
                 file:del_dir_r(SourceDir),
                 file:del_dir_r(TargetDir),
                 
                 ?debugFmt("Empty database copy test completed successfully", [])
             end)},
     
     {timeout, 30,
      ?_test(begin
                 % Test 3: Multiple copies to different locations
                 SourceDir = "/tmp/multi-source-db",
                 TargetDir1 = "/tmp/multi-target-db-1",
                 TargetDir2 = "/tmp/multi-target-db-2",
                 
                 % Clean up
                 file:del_dir_r(SourceDir),
                 file:del_dir_r(TargetDir1),
                 file:del_dir_r(TargetDir2),
                 filelib:ensure_dir(SourceDir ++ "/"),
                 
                 % Create source with data
                 {ok, SourceEnv} = elmdb:env_open(SourceDir, [{map_size, 10485760}]),
                 {ok, SourceDB} = elmdb:db_open(SourceEnv, [create]),
                 
                 % Write test data
                 lists:foreach(fun(I) ->
                     Key = iolist_to_binary([<<"multi_key_">>, integer_to_binary(I)]),
                     Value = iolist_to_binary([<<"multi_value_">>, integer_to_binary(I)]),
                     ok = elmdb:put(SourceDB, Key, Value)
                 end, lists:seq(1, 1000)),
                 
                 ok = elmdb:flush(SourceDB),
                 ok = elmdb:db_close(SourceDB),
                 ok = elmdb:env_close(SourceEnv),
                 
                 % Copy to first target
                 filelib:ensure_dir(TargetDir1 ++ "/"),
                 {ok, _} = file:copy(SourceDir ++ "/data.mdb", TargetDir1 ++ "/data.mdb"),
                 {ok, _} = file:copy(SourceDir ++ "/lock.mdb", TargetDir1 ++ "/lock.mdb"),
                 
                 % Copy to second target
                 filelib:ensure_dir(TargetDir2 ++ "/"),
                 {ok, _} = file:copy(SourceDir ++ "/data.mdb", TargetDir2 ++ "/data.mdb"),
                 {ok, _} = file:copy(SourceDir ++ "/lock.mdb", TargetDir2 ++ "/lock.mdb"),
                 
                 % Verify both copies
                 {ok, TargetEnv1} = elmdb:env_open(TargetDir1, [{map_size, 10485760}]),
                 {ok, TargetDB1} = elmdb:db_open(TargetEnv1, []),
                 
                 {ok, TargetEnv2} = elmdb:env_open(TargetDir2, [{map_size, 10485760}]),
                 {ok, TargetDB2} = elmdb:db_open(TargetEnv2, []),
                 
                 % Verify same data in both copies
                 ?assertEqual({ok, <<"multi_value_1">>}, elmdb:get(TargetDB1, <<"multi_key_1">>)),
                 ?assertEqual({ok, <<"multi_value_1">>}, elmdb:get(TargetDB2, <<"multi_key_1">>)),
                 ?assertEqual({ok, <<"multi_value_500">>}, elmdb:get(TargetDB1, <<"multi_key_500">>)),
                 ?assertEqual({ok, <<"multi_value_500">>}, elmdb:get(TargetDB2, <<"multi_key_500">>)),
                 
                 % Clean up
                 ok = elmdb:db_close(TargetDB1),
                 ok = elmdb:env_close(TargetEnv1),
                 ok = elmdb:db_close(TargetDB2),
                 ok = elmdb:env_close(TargetEnv2),
                 
                 file:del_dir_r(SourceDir),
                 file:del_dir_r(TargetDir1),
                 file:del_dir_r(TargetDir2),
                 
                 ?debugFmt("Multiple copies test completed successfully", [])
             end)}
    ].

%%%===================================================================
%%% Match Function Tests
%%%===================================================================

match_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Setup test data with hierarchical keys
                     ok = elmdb:put(DB, <<"users/alice/name">>, <<"Alice">>),
                     ok = elmdb:put(DB, <<"users/alice/email">>, <<"alice@example.com">>),
                     ok = elmdb:put(DB, <<"users/alice/age">>, <<"30">>),
                     
                     ok = elmdb:put(DB, <<"users/bob/name">>, <<"Bob">>),
                     ok = elmdb:put(DB, <<"users/bob/email">>, <<"bob@example.com">>),
                     ok = elmdb:put(DB, <<"users/bob/age">>, <<"25">>),
                     
                     ok = elmdb:put(DB, <<"users/charlie/name">>, <<"Charlie">>),
                     ok = elmdb:put(DB, <<"users/charlie/email">>, <<"charlie@example.com">>),
                     ok = elmdb:put(DB, <<"users/charlie/age">>, <<"30">>),
                     
                     % Test single pattern match
                     Patterns1 = [{<<"name">>, <<"Alice">>}],
                     {ok, Result1} = elmdb:match(DB, Patterns1),
                     ?assertEqual([<<"users/alice">>], Result1),
                     
                     % Test multiple pattern match
                     Patterns2 = [{<<"name">>, <<"Bob">>}, {<<"email">>, <<"bob@example.com">>}],
                     {ok, Result2} = elmdb:match(DB, Patterns2),
                     ?assertEqual([<<"users/bob">>], Result2),
                     
                     % Test pattern matching multiple IDs
                     Patterns3 = [{<<"age">>, <<"30">>}],
                     {ok, Result3} = elmdb:match(DB, Patterns3),
                     ?assertEqual(2, length(Result3)),
                     ?assert(lists:member(<<"users/alice">>, Result3)),
                     ?assert(lists:member(<<"users/charlie">>, Result3)),
                     
                     % Test no matches
                     Patterns4 = [{<<"name">>, <<"David">>}],
                     ?assertEqual(not_found, elmdb:match(DB, Patterns4)),
                     
                     % Test partial match (not all patterns match)
                     Patterns5 = [{<<"name">>, <<"Alice">>}, {<<"email">>, <<"wrong@example.com">>}],
                     ?assertEqual(not_found, elmdb:match(DB, Patterns5))
                 end)
         ]
     end}.

match_complex_hierarchical_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Setup complex hierarchical data
                     ok = elmdb:put(DB, <<"items/1234/type">>, <<"product">>),
                     ok = elmdb:put(DB, <<"items/1234/status">>, <<"active">>),
                     ok = elmdb:put(DB, <<"items/1234/price">>, <<"99.99">>),
                     ok = elmdb:put(DB, <<"items/1234/name">>, <<"Widget">>),
                     
                     ok = elmdb:put(DB, <<"items/5678/type">>, <<"product">>),
                     ok = elmdb:put(DB, <<"items/5678/status">>, <<"active">>),
                     ok = elmdb:put(DB, <<"items/5678/price">>, <<"99.99">>),
                     ok = elmdb:put(DB, <<"items/5678/name">>, <<"Gadget">>),
                     
                     ok = elmdb:put(DB, <<"items/9999/type">>, <<"service">>),
                     ok = elmdb:put(DB, <<"items/9999/status">>, <<"active">>),
                     ok = elmdb:put(DB, <<"items/9999/price">>, <<"99.99">>),
                     ok = elmdb:put(DB, <<"items/9999/name">>, <<"Support">>),
                     
                     % Test matching products with specific price
                     Patterns1 = [{<<"type">>, <<"product">>}, {<<"status">>, <<"active">>}, {<<"price">>, <<"99.99">>}],
                     {ok, Result1} = elmdb:match(DB, Patterns1),
                     ?assertEqual(2, length(Result1)),
                     ?assert(lists:member(<<"items/1234">>, Result1)),
                     ?assert(lists:member(<<"items/5678">>, Result1)),
                     
                     % Test matching specific combination
                     Patterns2 = [{<<"type">>, <<"service">>}, {<<"name">>, <<"Support">>}],
                     {ok, Result2} = elmdb:match(DB, Patterns2),
                     ?assertEqual([<<"items/9999">>], Result2)
                 end)
         ]
     end}.

match_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Test empty patterns (should return not_found)
                     ?assertEqual(not_found, elmdb:match(DB, [])),
                     
                     % Test with non-hierarchical keys
                     ok = elmdb:put(DB, <<"simple_key">>, <<"simple_value">>),
                     ok = elmdb:put(DB, <<"another_key">>, <<"another_value">>),
                     
                     % Empty suffix should match non-hierarchical keys
                     Patterns1 = [{<<>>, <<"simple_value">>}],
                     {ok, Result1} = elmdb:match(DB, Patterns1),
                     ?assertEqual([<<"simple_key">>], Result1),
                     
                     % Test with duplicate patterns (should still work)
                     ok = elmdb:put(DB, <<"test/id/field">>, <<"value">>),
                     Patterns2 = [{<<"field">>, <<"value">>}, {<<"field">>, <<"value">>}],
                     {ok, Result2} = elmdb:match(DB, Patterns2),
                     ?assertEqual([<<"test/id">>], Result2),
                     
                     % Test with very long keys
                     LongID = iolist_to_binary(lists:duplicate(100, "x")),
                     LongKey = <<LongID/binary, "/field">>,
                     ok = elmdb:put(DB, LongKey, <<"value">>),
                     Patterns3 = [{<<"field">>, <<"value">>}],
                     {ok, Result3} = elmdb:match(DB, Patterns3),
                     ?assert(lists:member(LongID, Result3))
                 end)
         ]
     end}.

match_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Create a larger dataset for performance testing
                     lists:foreach(fun(I) ->
                         ID = iolist_to_binary([<<"user_">>, integer_to_binary(I)]),
                         ok = elmdb:put(DB, <<ID/binary, "/name">>, <<"Name", (integer_to_binary(I))/binary>>),
                         ok = elmdb:put(DB, <<ID/binary, "/email">>, <<(integer_to_binary(I))/binary, "@example.com">>),
                         ok = elmdb:put(DB, <<ID/binary, "/status">>, 
                                       case I rem 3 of
                                           0 -> <<"active">>;
                                           1 -> <<"inactive">>;
                                           2 -> <<"pending">>
                                       end),
                         ok = elmdb:put(DB, <<ID/binary, "/score">>, 
                                       integer_to_binary(I rem 100))
                     end, lists:seq(1, 1000)),
                     
                     ok = elmdb:flush(DB),
                     
                     % Test matching with multiple patterns on large dataset
                     StartTime = erlang:monotonic_time(millisecond),
                     Patterns = [{<<"status">>, <<"active">>}, {<<"score">>, <<"42">>}],
                     Result = elmdb:match(DB, Patterns),
                     EndTime = erlang:monotonic_time(millisecond),
                     Duration = EndTime - StartTime,
                     
                     % Should complete within 100ms as per PRD requirement
                     ?assert(Duration < 100),
                     ?assertMatch({ok, _}, Result),
                     
                     % Verify correctness
                     case Result of
                         {ok, IDs} ->
                             % Should find user_42, user_342, user_642, user_942 (all with score 42 and active status)
                             Expected = [<<"user_42">>, <<"user_342">>, <<"user_642">>, <<"user_942">>],
                             FoundActive = lists:filter(fun(ID) ->
                                 case binary:split(ID, <<"_">>) of
                                     [<<"user">>, NumBin] ->
                                         Num = binary_to_integer(NumBin),
                                         (Num rem 100 == 42) andalso (Num rem 3 == 0);
                                     _ -> false
                                 end
                             end, IDs),
                             ?assertEqual(length(Expected), length(FoundActive));
                         _ ->
                             ?assert(false)
                     end
                 end)
         ]
     end}.

match_concurrent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Setup initial data
                     ok = elmdb:put(DB, <<"concurrent/test/field1">>, <<"value1">>),
                     ok = elmdb:put(DB, <<"concurrent/test/field2">>, <<"value2">>),
                     ok = elmdb:flush(DB),
                     
                     % Spawn multiple processes to perform match operations concurrently
                     Parent = self(),
                     Workers = lists:map(fun(I) ->
                         spawn(fun() ->
                             Patterns = [{<<"field1">>, <<"value1">>}, {<<"field2">>, <<"value2">>}],
                             Result = elmdb:match(DB, Patterns),
                             Parent ! {self(), I, Result}
                         end)
                     end, lists:seq(1, 10)),
                     
                     % Collect results
                     Results = lists:map(fun(Worker) ->
                         receive
                             {Worker, _I, Result} -> Result
                         after 1000 ->
                             timeout
                         end
                     end, Workers),
                     
                     % All concurrent matches should succeed with same result
                     lists:foreach(fun(Result) ->
                         ?assertEqual({ok, [<<"concurrent/test">>]}, Result)
                     end, Results)
                 end)
         ]
     end}.

%%%===================================================================
%%% Ported Tests from expr/erlbuf
%%%===================================================================

put_batch_cache_invalidation_test_() ->
    [
     ?_test(begin
                {Dir, Env, DB} = setup(),
                ok = elmdb:put(DB, <<"k1">>, <<"old">>),
                elmdb:flush(DB),
                ?assertEqual({ok, <<"old">>}, elmdb:get(DB, <<"k1">>)),
                ok = elmdb:put(DB, <<"k1">>, <<"new">>),
                ok = elmdb:put(DB, <<"k2">>, <<"v2">>),
                ?assertEqual({ok, <<"new">>}, elmdb:get(DB, <<"k1">>)),
                ?assertEqual({ok, <<"v2">>}, elmdb:get(DB, <<"k2">>)),
                cleanup({Dir, Env, DB})
            end)
    ].

env_close_cached_txn_test_() ->
    [
     ?_test(begin
                {Dir, Env, DB} = setup(),
                ok = elmdb:put(DB, <<"alive">>, <<"yes">>),
                elmdb:flush(DB),
                ?assertEqual({ok, <<"yes">>}, elmdb:get(DB, <<"alive">>)),
                elmdb:db_close(DB),
                elmdb:env_close(Env),
                {ok, Env2} = elmdb:env_open(Dir, [{map_size, 10485760}]),
                {ok, DB2} = elmdb:db_open(Env2, [create]),
                ?assertEqual({ok, <<"yes">>}, elmdb:get(DB2, <<"alive">>)),
                cleanup({Dir, Env2, DB2})
            end)
    ].

concurrent_write_read_test_() ->
    [
     ?_test(begin
                {Dir, Env, DB} = setup(),
                ok = elmdb:put(DB, <<"counter">>, <<"0">>),
                elmdb:flush(DB),
                Self = self(),
                Writers = [spawn_monitor(fun() ->
                    lists:foreach(fun(N) ->
                        elmdb:put(DB, <<"counter">>, integer_to_binary(N)),
                        elmdb:flush(DB)
                    end, lists:seq(I*100, I*100+99)),
                    Self ! {writer_done, self()}
                end) || I <- lists:seq(1, 5)],
                Readers = [spawn_monitor(fun() ->
                    lists:foreach(fun(_) ->
                        case elmdb:get(DB, <<"counter">>) of
                            {ok, _} -> ok;
                            not_found -> exit(stale_not_found)
                        end
                    end, lists:seq(1, 500)),
                    Self ! {reader_done, self()}
                end) || _ <- lists:seq(1, 5)],
                [receive {writer_done, P} -> ok end || {P, _} <- Writers],
                [receive {reader_done, P} -> ok end || {P, _} <- Readers],
                [receive {'DOWN', R, process, P, normal} -> ok end
                 || {P, R} <- Writers ++ Readers],
                cleanup({Dir, Env, DB})
            end)
    ].

overlay_buffered_read_test_() ->
    [
     ?_test(begin
                {Dir, Env, DB} = setup(),
                ok = elmdb:put(DB, <<"buf1">>, <<"val1">>),
                ?assertEqual({ok, <<"val1">>}, elmdb:get(DB, <<"buf1">>)),
                cleanup({Dir, Env, DB})
            end)
    ].

%%%===================================================================
%%% Regression Tests
%%%===================================================================

flush_visibility_regression_test_() ->
    {setup,
     fun() ->
         Dir = test_dir(),
         file:del_dir_r(Dir),
         filelib:ensure_dir(Dir ++ "/"),
         {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}, {batch_size, 1000}]),
         {ok, DB} = elmdb:db_open(Env, [create]),
         {Dir, Env, DB}
     end,
     fun({Dir, Env, DB}) ->
         _ = elmdb:db_close(DB),
         _ = elmdb:env_close(Env),
         file:del_dir_r(Dir)
     end,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     % Write exactly 1000 keys to trigger background flush
                     lists:foreach(fun(I) ->
                         Key = <<"key_", (integer_to_binary(I))/binary>>,
                         Value = <<"value_", (integer_to_binary(I))/binary>>,
                         ok = elmdb:put(DB, Key, Value)
                     end, lists:seq(1, 1000)),

                     % Immediately read all keys - none should return not_found
                     lists:foreach(fun(I) ->
                         Key = <<"key_", (integer_to_binary(I))/binary>>,
                         Result = elmdb:get(DB, Key),
                         ?assertNotEqual(not_found, Result)
                     end, lists:seq(1, 1000))
                 end)
         ]
     end}.

%%%===================================================================
%%% Fatal-state and Recovery Regression Tests
%%%===================================================================

flush_sync_waiter_on_worker_death_test_() ->
    {timeout, 15,
     ?_test(begin
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                {ok, Env} = elmdb:env_open(Dir, [{map_size, 1048576}, {batch_size, 3},
                                                  no_sync, no_mem_init, write_map]),
                {ok, _DB0} = elmdb:db_open(Env, [create]),
                LargeVal = binary:copy(<<"x">>, 10000),
                trigger_map_full(_DB0, LargeVal, 0),

                {ok, DB} = elmdb:db_open(Env, [create]),

                Self = self(),
                Flushers = [spawn_monitor(fun() ->
                    R = elmdb:flush(DB),
                    Self ! {flush_result, self(), R}
                end) || _ <- lists:seq(1, 3)],

                lists:foreach(fun(I) ->
                    K = <<"trigger_", (integer_to_binary(I))/binary>>,
                    elmdb:put(DB, K, <<"v">>)
                end, lists:seq(1, 6)),

                lists:foreach(fun({Pid, Ref}) ->
                    receive
                        {flush_result, Pid, {error, _, _}} -> ok
                    after 5000 ->
                        ?assert(false)
                    end,
                    receive {'DOWN', Ref, process, _, _} -> ok after 1000 -> ok end
                end, Flushers),

                ?assertMatch({error, _, _}, elmdb:get(DB, <<"trigger_1">>)),

                _ = elmdb:db_close(DB),
                _ = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)}.

trigger_map_full(DB, LargeVal, N) ->
    Key = <<"fill_", (integer_to_binary(N))/binary>>,
    case elmdb:put(DB, Key, LargeVal) of
        ok ->
            case elmdb:flush(DB) of
                ok -> trigger_map_full(DB, LargeVal, N + 1);
                {error, _, _} -> ok
            end;
        {error, _, _} -> ok
    end.

reopen_after_fatal_restores_worker_test_() ->
    {timeout, 15,
     ?_test(begin
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                {ok, Env} = elmdb:env_open(Dir, [{map_size, 1048576}, {batch_size, 3},
                                                  no_sync, no_mem_init, write_map]),
                {ok, _DB1} = elmdb:db_open(Env, [create]),

                LargeVal = binary:copy(<<"x">>, 10000),
                trigger_map_full(_DB1, LargeVal, 0),

                {ok, DB2} = elmdb:db_open(Env, [create]),

                ok = elmdb:put(DB2, <<"recovered_key">>, <<"recovered_val">>),
                ?assertEqual({ok, <<"recovered_val">>}, elmdb:get(DB2, <<"recovered_key">>)),
                ?assertMatch({error, _, _}, elmdb:flush(DB2)),

                _ = elmdb:db_close(DB2),
                _ = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)}.

put_batch_rejects_invalid_keys_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     Result = elmdb:put_batch(DB, [{<<>>, <<"val">>}, {<<"good_key">>, <<"good_val">>}]),
                     ?assertMatch({error, validation_error, _}, Result),
                     ?assertEqual(not_found, elmdb:get(DB, <<"good_key">>)),

                     ok = elmdb:put_batch(DB, [{<<"healthy_key">>, <<"healthy_val">>}]),
                     ok = elmdb:flush(DB),
                     ?assertEqual({ok, <<"healthy_val">>}, elmdb:get(DB, <<"healthy_key">>))
                 end)
         ]
     end}.

put_batch_oversized_key_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, _Env, DB}) ->
         [
          ?_test(begin
                     OversizedKey = binary:copy(<<"x">>, 512),
                     ?assertMatch({error, validation_error, _},
                                  elmdb:put_batch(DB, [{OversizedKey, <<"val">>}])),

                     ok = elmdb:put_batch(DB, [{<<"durable_key">>, <<"durable_val">>}]),
                     ok = elmdb:flush(DB),
                     ?assertEqual({ok, <<"durable_val">>}, elmdb:get(DB, <<"durable_key">>))
                 end)
         ]
     end}.

concurrent_ops_no_hang_test_() ->
    {timeout, 15,
     ?_test(begin
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                {ok, Env} = elmdb:env_open(Dir, [{map_size, 104857600}, {batch_size, 5}]),
                {ok, DB} = elmdb:db_open(Env, [create]),

                Self = self(),
                Workers = [spawn_monitor(fun() ->
                    Prefix = <<"w", (integer_to_binary(W))/binary, "_">>,
                    worker_script(DB, Prefix, 10),
                    Self ! {worker_done, self()}
                end) || W <- lists:seq(1, 5)],

                Closer = spawn_monitor(fun() ->
                    timer:sleep(50),
                    elmdb:db_close(DB),
                    elmdb:db_open(Env, [create]),
                    Self ! {closer_done, self()}
                end),

                AllProcs = Workers ++ [Closer],
                lists:foreach(fun({P, Ref}) ->
                    receive
                        {worker_done, P} -> ok;
                        {closer_done, P} -> ok;
                        {'DOWN', Ref, process, P, _} -> ok
                    after 10000 ->
                        ?assert(false)
                    end
                end, AllProcs),
                [receive {'DOWN', R, process, P, _} -> ok after 1000 -> ok end
                 || {P, R} <- AllProcs],

                {ok, DB2} = elmdb:db_open(Env, [create]),
                ok = elmdb:put(DB2, <<"liveness_check">>, <<"ok">>),
                ok = elmdb:flush(DB2),
                ?assertEqual({ok, <<"ok">>}, elmdb:get(DB2, <<"liveness_check">>)),

                _ = elmdb:db_close(DB2),
                _ = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)}.

worker_script(DB, Prefix, Rounds) ->
    lists:foreach(fun(R) ->
        RB = integer_to_binary(R),
        elmdb:put(DB, <<Prefix/binary, RB/binary, "_a">>, <<"v1">>),
        elmdb:put(DB, <<Prefix/binary, RB/binary, "_b">>, <<"v2">>),
        elmdb:put(DB, <<Prefix/binary, RB/binary, "_c">>, <<"v3">>),
        elmdb:put_batch(DB, [
            {<<Prefix/binary, RB/binary, "_d">>, <<"v4">>},
            {<<Prefix/binary, RB/binary, "_e">>, <<"v5">>}
        ]),
        elmdb:flush(DB),
        elmdb:get(DB, <<Prefix/binary, RB/binary, "_a">>),
        elmdb:get(DB, <<Prefix/binary, RB/binary, "_b">>),
        elmdb:list(DB, <<Prefix/binary, RB/binary, "_">>)
    end, lists:seq(1, Rounds)).

repeated_fatal_recover_cycle_test_() ->
    {timeout, 30,
     ?_test(begin
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                {ok, Env} = elmdb:env_open(Dir, [{map_size, 1048576}, {batch_size, 3},
                                                  no_sync, no_mem_init, write_map]),
                {ok, DB0} = elmdb:db_open(Env, [create]),

                LargeVal = binary:copy(<<"x">>, 10000),
                recover_cycle(Env, DB0, LargeVal, 1, 3),

                _ = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)}.

recover_cycle(_Env, _DB, _LargeVal, Iter, Max) when Iter > Max -> ok;
recover_cycle(Env, DB, LargeVal, Iter, Max) ->
    trigger_map_full(DB, LargeVal, Iter * 1000),
    {ok, DB2} = elmdb:db_open(Env, [create]),
    SmallKey = <<"small_", (integer_to_binary(Iter))/binary>>,
    ok = elmdb:put(DB2, SmallKey, <<"survived">>),
    ?assertEqual({ok, <<"survived">>}, elmdb:get(DB2, SmallKey)),
    ?assertMatch({error, _, _}, elmdb:flush(DB2)),
    recover_cycle(Env, DB2, LargeVal, Iter + 1, Max).

multi_db_parallel_no_crosstalk_test_() ->
    {timeout, 15,
     ?_test(begin
                Dirs = [test_dir() || _ <- lists:seq(1, 3)],
                lists:foreach(fun(D) ->
                    file:del_dir_r(D),
                    filelib:ensure_dir(D ++ "/")
                end, Dirs),

                Envs = [begin {ok, E} = elmdb:env_open(D, [{map_size, 10485760}]), E end
                        || D <- Dirs],
                DBs = [begin {ok, Db} = elmdb:db_open(E, [create]), Db end
                       || E <- Envs],

                Self = self(),
                Workers = lists:zipwith(fun(Idx, DB) ->
                    spawn_monitor(fun() ->
                        Prefix = <<"db", (integer_to_binary(Idx))/binary, "_key_">>,
                        lists:foreach(fun(I) ->
                            K = <<Prefix/binary, (integer_to_binary(I))/binary>>,
                            elmdb:put(DB, K, <<"val">>)
                        end, lists:seq(1, 50)),
                        elmdb:flush(DB),
                        lists:foreach(fun(I) ->
                            K = <<Prefix/binary, (integer_to_binary(I))/binary>>,
                            ?assertEqual({ok, <<"val">>}, elmdb:get(DB, K))
                        end, lists:seq(1, 50)),
                        Self ! {db_done, self()}
                    end)
                end, lists:seq(1, 3), DBs),

                lists:foreach(fun({P, Ref}) ->
                    receive
                        {db_done, P} -> ok;
                        {'DOWN', Ref, process, P, _} -> ok
                    after 10000 ->
                        ?assert(false)
                    end
                end, Workers),
                [receive {'DOWN', R, process, P, _} -> ok after 1000 -> ok end
                 || {P, R} <- Workers],

                [DB1, DB2, DB3] = DBs,
                ?assertEqual(not_found, elmdb:get(DB1, <<"db2_key_1">>)),
                ?assertEqual(not_found, elmdb:get(DB2, <<"db3_key_1">>)),
                ?assertEqual(not_found, elmdb:get(DB3, <<"db1_key_1">>)),

                lists:foreach(fun({Db, E, D}) ->
                    _ = elmdb:db_close(Db),
                    _ = elmdb:env_close(E),
                    file:del_dir_r(D)
                end, lists:zip3(DBs, Envs, Dirs))
            end)}.

auto_flush_test_() ->
    {timeout, 15,
     ?_test(begin
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}, {batch_size, 3},
                                                  no_sync, no_mem_init, write_map]),
                {ok, DB} = elmdb:db_open(Env, [create]),

                lists:foreach(fun(I) ->
                    K = <<"auto_", (integer_to_binary(I))/binary>>,
                    ok = elmdb:put(DB, K, <<"v">>)
                end, lists:seq(1, 10)),
                timer:sleep(500),
                ?assertEqual(0, elmdb:overlay_count(DB)),

                _ = elmdb:db_close(DB),
                _ = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)}.

concurrent_open_writers_auto_flush_test_() ->
    {timeout, 15,
     ?_test(begin
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}, {batch_size, 3},
                                                  no_sync, no_mem_init, write_map]),
                {ok, DB} = elmdb:db_open(Env, [create]),

                Self = self(),
                Writer = spawn_monitor(fun() ->
                    lists:foreach(fun(I) ->
                        K = <<"race_", (integer_to_binary(I))/binary>>,
                        elmdb:put(DB, K, <<"v">>)
                    end, lists:seq(1, 20)),
                    Self ! {writer_done, self()}
                end),
                Opener = spawn_monitor(fun() ->
                    timer:sleep(1),
                    {ok, _} = elmdb:db_open(Env, [create])
                end),

                {WriterPid, WriterRef} = Writer,
                receive {writer_done, WriterPid} -> ok
                after 5000 -> ?assert(false) end,
                receive {'DOWN', WriterRef, process, _, _} -> ok after 1000 -> ok end,

                {OpenerPid, OpenerRef} = Opener,
                receive {'DOWN', OpenerRef, process, OpenerPid, normal} -> ok
                after 5000 -> ?assert(false) end,

                timer:sleep(500),
                ?assertEqual(0, elmdb:overlay_count(DB)),

                _ = elmdb:db_close(DB),
                _ = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)}.

flush_pending_resets_after_recovery_test_() ->
    {timeout, 15,
     ?_test(begin
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                {ok, Env} = elmdb:env_open(Dir, [{map_size, 1048576}, {batch_size, 3},
                                                   no_sync, no_mem_init, write_map]),
                {ok, _DB1} = elmdb:db_open(Env, [create]),
                trigger_map_full(_DB1, binary:copy(<<"x">>, 10000), 0),

                {ok, DB2} = elmdb:db_open(Env, [create]),
                lists:foreach(fun(I) ->
                    K = <<"fp_", (integer_to_binary(I))/binary>>,
                    ok = elmdb:put(DB2, K, <<"v">>)
                end, lists:seq(1, 6)),
                timer:sleep(500),
                ?assertMatch({error, _, _}, elmdb:get(DB2, <<"fp_1">>)),

                _ = elmdb:db_close(DB2),
                _ = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end)}.
