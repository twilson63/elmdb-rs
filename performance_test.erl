#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/elmdb/ebin

-mode(compile).

main(_) ->
    application:ensure_all_started(elmdb),
    
    % Open database
    DbPath = "/tmp/performance_test_db",
    os:cmd("rm -rf " ++ DbPath),
    os:cmd("mkdir -p " ++ DbPath),
    {ok, Env} = elmdb:env_open(DbPath, [{map_size, 1024*1024*1024}]),
    {ok, Db} = elmdb:db_open(Env, [create]),
    
    io:format("Starting performance tests...~n"),
    
    % Test 1: Write performance (baseline - not measuring this)
    io:format("Setting up test data...~n"),
    setup_test_data(Db, 1000),
    
    % Test 2: Read performance
    io:format("Testing read performance...~n"),
    test_read_performance(Db, 10000),
    
    % Test 3: List performance  
    io:format("Testing list performance...~n"),
    test_list_performance(Db, 1000),
    
    % Cleanup
    elmdb:env_close(Env),
    os:cmd("rm -rf " ++ DbPath),
    
    io:format("Performance tests completed.~n").

setup_test_data(Db, NumGroups) ->
    % Create hierarchical data structure like: group1/msg1, group1/msg2, etc.
    lists:foreach(fun(GroupNum) ->
        GroupKey = "group" ++ integer_to_list(GroupNum),
        lists:foreach(fun(MsgNum) ->
            Key = list_to_binary(GroupKey ++ "/msg" ++ integer_to_list(MsgNum)),
            Value = list_to_binary("message_data_" ++ integer_to_list(MsgNum)),
            elmdb:put(Db, Key, Value)
        end, lists:seq(1, 10))
    end, lists:seq(1, NumGroups)),
    
    % Flush to ensure all writes are committed
    elmdb:flush(Db).

test_read_performance(Db, NumReads) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    % Perform many get operations
    lists:foreach(fun(N) ->
        GroupNum = (N rem 1000) + 1,
        MsgNum = (N rem 10) + 1,
        Key = list_to_binary("group" ++ integer_to_list(GroupNum) ++ "/msg" ++ integer_to_list(MsgNum)),
        case elmdb:get(Db, Key) of
            {ok, _Value} -> ok;
            not_found -> ok;
            Error -> io:format("Read error: ~p~n", [Error])
        end
    end, lists:seq(1, NumReads)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    Rate = (NumReads * 1000) div max(Duration, 1),
    io:format("Read Performance: ~p reads in ~p ms (~p reads/s)~n", [NumReads, Duration, Rate]).

test_list_performance(Db, NumLists) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    % Perform many list operations
    lists:foreach(fun(N) ->
        GroupNum = (N rem 1000) + 1,
        Prefix = list_to_binary("group" ++ integer_to_list(GroupNum)),
        case elmdb:list(Db, Prefix) of
            {ok, _Children} -> ok;
            not_found -> ok;
            Error -> io:format("List error: ~p~n", [Error])
        end
    end, lists:seq(1, NumLists)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    Rate = (NumLists * 1000) div max(Duration, 1),
    io:format("List Performance: ~p lists in ~p ms (~p lists/s)~n", [NumLists, Duration, Rate]).