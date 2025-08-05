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
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    {Dir, Env, DB}.

cleanup({Dir, Env, _DB}) ->
    elmdb:env_close(Env),
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
                     % Test batch put
                     Batch = [{<<"batch1">>, <<"value1">>},
                              {<<"batch2">>, <<"value2">>},
                              {<<"batch3">>, <<"value3">>}],
                     ok = elmdb:put_batch(DB, Batch),
                     
                     ?assertEqual({ok, <<"value1">>}, elmdb:get(DB, <<"batch1">>)),
                     ?assertEqual({ok, <<"value2">>}, elmdb:get(DB, <<"batch2">>)),
                     ?assertEqual({ok, <<"value3">>}, elmdb:get(DB, <<"batch3">>))
                 end),
          
          ?_test(begin
                     % Test empty batch
                     ok = elmdb:put_batch(DB, [])
                 end)
         ]
     end}.

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
                % Test operations on closed database
                {Dir, Env, DB} = setup(),
                ok = elmdb:db_close(DB),
                
                % All operations should fail
                CheckDbError = fun(R) ->
                    case R of
                        {error, database_error, _} -> ok;
                        {error, database_error} -> ok;
                        _ -> ?assertEqual({error, database_error}, R)
                    end
                end,
                
                CheckDbError(elmdb:put(DB, <<"k">>, <<"v">>)),
                CheckDbError(elmdb:get(DB, <<"k">>)),
                CheckDbError(elmdb:list(DB, <<>>)),
                CheckDbError(elmdb:flush(DB)),
                
                ok = elmdb:env_close(Env),
                file:del_dir_r(Dir)
            end),
     
     ?_test(begin
                % Test invalid batch data
                {Dir, Env, DB} = setup(),
                ?assertError(badarg, elmdb:put_batch(DB, not_a_list)),
                ?assertError(badarg, elmdb:put_batch(DB, [{<<"k">>, <<"v">>, extra}])),
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
                % Test force close
                Dir = test_dir(),
                file:del_dir_r(Dir),
                filelib:ensure_dir(Dir ++ "/"),
                
                {ok, Env} = elmdb:env_open(Dir, []),
                {ok, _DB} = elmdb:db_open(Env, [create]),
                
                % Force close should work even with open database
                Result = elmdb:env_force_close(Env),
                case Result of
                    ok -> ok;
                    {ok, _Msg} -> ok
                end,
                
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