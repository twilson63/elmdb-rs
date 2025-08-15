#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc
%%% ElmDB Chaos Test Suite
%%% 
%%% Tests system stability under various stress conditions:
%%% - Concurrent reads and writes
%%% - Mixed operations including the new match function
%%% - Large dataset operations
%%% - Rapid environment open/close cycles
%%% - Memory pressure tests
%%% @end
%%%-------------------------------------------------------------------

main(_Args) ->
    io:format("~n=== ElmDB Chaos Test Suite ===~n~n"),
    
    % Load the elmdb module
    case code:ensure_loaded(elmdb) of
        {module, elmdb} ->
            run_chaos_tests();
        {error, Reason} ->
            io:format("ERROR: Failed to load elmdb module: ~p~n", [Reason]),
            halt(1)
    end.

run_chaos_tests() ->
    TestDir = "/tmp/elmdb_chaos_" ++ integer_to_list(erlang:system_time()),
    ok = filelib:ensure_dir(filename:join(TestDir, "dummy")),
    
    io:format("Test directory: ~s~n~n", [TestDir]),
    
    % Run all chaos tests
    Tests = [
        {"Concurrent Mixed Operations", fun() -> test_concurrent_mixed(TestDir) end},
        {"Rapid Open/Close Cycles", fun() -> test_rapid_open_close(TestDir) end},
        {"Large Dataset with Match", fun() -> test_large_dataset_match(TestDir) end},
        {"Memory Pressure Test", fun() -> test_memory_pressure(TestDir) end},
        {"Pattern Match Stress", fun() -> test_match_stress(TestDir) end}
    ],
    
    Results = lists:map(fun({Name, Test}) ->
        io:format("~n--- Running: ~s ---~n", [Name]),
        try
            {Time, _Result} = timer:tc(Test),
            io:format("✓ Completed in ~.3f seconds~n", [Time/1000000]),
            {Name, pass, Time}
        catch
            Type:Error ->
                io:format("✗ FAILED: ~p:~p~n", [Type, Error]),
                {Name, fail, {Type, Error}}
        end
    end, Tests),
    
    % Print summary
    io:format("~n=== CHAOS TEST SUMMARY ===~n"),
    Passed = length([ok || {_, pass, _} <- Results]),
    Failed = length([ok || {_, fail, _} <- Results]),
    io:format("Passed: ~p/~p~n", [Passed, length(Results)]),
    io:format("Failed: ~p/~p~n", [Failed, length(Results)]),
    
    % Cleanup
    file:del_dir_r(TestDir),
    
    if Failed > 0 -> halt(1); true -> halt(0) end.

%% Test 1: Concurrent mixed operations
test_concurrent_mixed(BaseDir) ->
    Dir = filename:join(BaseDir, "concurrent"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 104857600}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Spawn multiple processes doing different operations
    NumWorkers = 50,
    OpsPerWorker = 100,
    
    Parent = self(),
    _Workers = lists:map(fun(WorkerId) ->
        spawn(fun() ->
            rand:seed(exsplus, {erlang:system_time(), erlang:unique_integer(), WorkerId}),
            lists:foreach(fun(Op) ->
                Key = iolist_to_binary([<<"key_">>, integer_to_binary(WorkerId), <<"_">>, integer_to_binary(Op)]),
                Value = iolist_to_binary([<<"value_">>, integer_to_binary(rand:uniform(1000))]),
                
                case rand:uniform(4) of
                    1 -> % Write
                        elmdb:put(DB, Key, Value);
                    2 -> % Read
                        elmdb:get(DB, Key);
                    3 -> % List
                        elmdb:list(DB, <<"key_">>);
                    4 -> % Match (new function)
                        Patterns = [{<<"suffix">>, Value}],
                        elmdb:match(DB, Patterns)
                end
            end, lists:seq(1, OpsPerWorker)),
            Parent ! {done, WorkerId}
        end)
    end, lists:seq(1, NumWorkers)),
    
    % Wait for all workers
    lists:foreach(fun(WorkerId) ->
        receive {done, WorkerId} -> ok
        after 10000 -> throw({timeout, WorkerId})
        end
    end, lists:seq(1, NumWorkers)),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  Completed ~p concurrent workers with ~p ops each~n", [NumWorkers, OpsPerWorker]).

%% Test 2: Rapid open/close cycles
test_rapid_open_close(BaseDir) ->
    lists:foreach(fun(Cycle) ->
        Dir = filename:join(BaseDir, "cycle_" ++ integer_to_list(Cycle)),
        ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
        
        {ok, Env} = elmdb:env_open(Dir, [{map_size, 10485760}]),
        {ok, DB} = elmdb:db_open(Env, [create]),
        
        % Quick operations
        elmdb:put(DB, <<"test">>, <<"data">>),
        {ok, <<"data">>} = elmdb:get(DB, <<"test">>),
        
        elmdb:db_close(DB),
        elmdb:env_close(Env)
    end, lists:seq(1, 100)),
    io:format("  Completed 100 rapid open/close cycles~n").

%% Test 3: Large dataset with match function
test_large_dataset_match(BaseDir) ->
    Dir = filename:join(BaseDir, "large_match"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 209715200}]), % 200MB
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Create hierarchical data
    NumEntities = 1000,
    lists:foreach(fun(EntityId) ->
        Entity = iolist_to_binary([<<"entity_">>, integer_to_binary(EntityId)]),
        elmdb:put(DB, <<Entity/binary, "/name">>, <<"Name", (integer_to_binary(EntityId))/binary>>),
        elmdb:put(DB, <<Entity/binary, "/type">>, 
            case EntityId rem 3 of
                0 -> <<"typeA">>;
                1 -> <<"typeB">>;
                2 -> <<"typeC">>
            end),
        elmdb:put(DB, <<Entity/binary, "/status">>, <<"active">>),
        elmdb:put(DB, <<Entity/binary, "/value">>, integer_to_binary(EntityId rem 100))
    end, lists:seq(1, NumEntities)),
    
    % Test various match patterns
    Patterns1 = [{<<"type">>, <<"typeA">>}, {<<"status">>, <<"active">>}],
    {ok, _Results1} = elmdb:match(DB, Patterns1),
    
    Patterns2 = [{<<"value">>, <<"42">>}],
    _Result2 = elmdb:match(DB, Patterns2),
    
    Patterns3 = [{<<"type">>, <<"typeB">>}, {<<"value">>, <<"0">>}],
    _Result3 = elmdb:match(DB, Patterns3),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  Created ~p entities with hierarchical data~n", [NumEntities]),
    io:format("  Match operations completed successfully~n").

%% Test 4: Memory pressure test
test_memory_pressure(BaseDir) ->
    Dir = filename:join(BaseDir, "memory_pressure"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 524288000}]), % 500MB
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Write large values
    LargeValue = list_to_binary(lists:duplicate(10000, $X)), % 10KB value
    lists:foreach(fun(I) ->
        Key = iolist_to_binary([<<"large_">>, integer_to_binary(I)]),
        elmdb:put(DB, Key, LargeValue)
    end, lists:seq(1, 1000)),
    
    % Force flush
    elmdb:flush(DB),
    
    % Read them back
    lists:foreach(fun(I) ->
        Key = iolist_to_binary([<<"large_">>, integer_to_binary(I)]),
        {ok, _} = elmdb:get(DB, Key)
    end, lists:seq(1, 100)),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  Handled 1000 large values (10KB each)~n").

%% Test 5: Pattern match stress test
test_match_stress(BaseDir) ->
    Dir = filename:join(BaseDir, "match_stress"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    {ok, Env} = elmdb:env_open(Dir, [{map_size, 104857600}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    % Create complex hierarchical structure
    lists:foreach(fun(Org) ->
        OrgBin = iolist_to_binary([<<"org_">>, integer_to_binary(Org)]),
        lists:foreach(fun(User) ->
            UserBin = iolist_to_binary([OrgBin, <<"/user_">>, integer_to_binary(User)]),
            lists:foreach(fun(Attr) ->
                AttrName = case Attr of
                    1 -> <<"name">>;
                    2 -> <<"email">>;
                    3 -> <<"role">>;
                    4 -> <<"status">>;
                    5 -> <<"department">>
                end,
                AttrValue = iolist_to_binary([AttrName, <<"_value_">>, integer_to_binary(rand:uniform(10))]),
                elmdb:put(DB, <<UserBin/binary, "/", AttrName/binary>>, AttrValue)
            end, lists:seq(1, 5))
        end, lists:seq(1, 20))
    end, lists:seq(1, 10)),
    
    % Complex match patterns
    ComplexPatterns = [
        {<<"name">>, <<"name_value_5">>},
        {<<"role">>, <<"role_value_5">>},
        {<<"status">>, <<"status_value_5">>}
    ],
    
    % Run multiple concurrent match operations
    Parent = self(),
    lists:foreach(fun(I) ->
        spawn(fun() ->
            Result = elmdb:match(DB, ComplexPatterns),
            Parent ! {match_done, I, Result}
        end)
    end, lists:seq(1, 20)),
    
    % Collect results
    lists:foreach(fun(I) ->
        receive {match_done, I, _Result} -> ok
        after 5000 -> throw({match_timeout, I})
        end
    end, lists:seq(1, 20)),
    
    elmdb:db_close(DB),
    elmdb:env_close(Env),
    io:format("  Created 10 orgs × 20 users × 5 attributes = 1000 keys~n"),
    io:format("  Completed 20 concurrent complex match operations~n").