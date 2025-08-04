#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/elmdb/ebin

-mode(compile).

main(_) ->
    TestDir = "/tmp/elmdb_message_test",
    os:cmd("rm -rf " ++ TestDir),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    
    {ok, Env} = elmdb:env_open(TestDir, [
        {map_size, 10*1024*1024*1024},  % 10GB
        no_sync,
        no_mem_init,
        write_map
    ]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    
    io:format("Message-style workload performance test~n"),
    io:format("=======================================~n~n"),
    
    % Create groups (like the C NIF test)
    io:format("Creating 100,000 groups...~n"),
    GroupStart = erlang:monotonic_time(millisecond),
    lists:foreach(fun(I) ->
        GroupId = integer_to_binary(I),
        % Each group has metadata
        elmdb:put(DB, <<"groups/", GroupId/binary, "/name">>, <<"Group ", GroupId/binary>>),
        elmdb:put(DB, <<"groups/", GroupId/binary, "/created">>, integer_to_binary(erlang:system_time(second))),
        elmdb:put(DB, <<"groups/", GroupId/binary, "/owner">>, <<"user", (integer_to_binary(I rem 1000))/binary>>)
    end, lists:seq(1, 100000)),
    GroupEnd = erlang:monotonic_time(millisecond),
    GroupTime = GroupEnd - GroupStart,
    io:format("Created 100,000 groups in ~pms (~p groups/s)~n", 
              [GroupTime, 100000000 div GroupTime]),
    
    % List groups (like the C NIF test)
    io:format("~nListing groups...~n"),
    ListStart = erlang:monotonic_time(millisecond),
    
    % List in batches like real workload
    TotalListed = lists:foldl(fun(Batch, Acc) ->
        StartIdx = Batch * 1000,
        lists:foreach(fun(I) ->
            GroupId = integer_to_binary(StartIdx + I),
            {ok, _Fields} = elmdb:list(DB, <<"groups/", GroupId/binary, "/">>)
        end, lists:seq(1, 1000)),
        Acc + 1000
    end, 0, lists:seq(0, 99)),
    
    ListEnd = erlang:monotonic_time(millisecond),
    ListTime = ListEnd - ListStart,
    io:format("Listed ~p groups in ~pms (~p groups/s)~n", 
              [TotalListed, ListTime, (TotalListed * 1000) div ListTime]),
    
    % Create messages
    io:format("~nCreating messages...~n"),
    MsgStart = erlang:monotonic_time(millisecond),
    lists:foreach(fun(I) ->
        MsgId = integer_to_binary(I),
        % Message structure similar to real workload
        elmdb:put(DB, <<"messages/", MsgId/binary, "/from">>, <<"user", (integer_to_binary(I rem 100))/binary>>),
        elmdb:put(DB, <<"messages/", MsgId/binary, "/to">>, <<"user", (integer_to_binary((I+1) rem 100))/binary>>),
        elmdb:put(DB, <<"messages/", MsgId/binary, "/subject">>, <<"Test message ", MsgId/binary>>),
        elmdb:put(DB, <<"messages/", MsgId/binary, "/body">>, 
                  iolist_to_binary(lists:duplicate(100, <<"This is message content. ">>))),
        elmdb:put(DB, <<"messages/", MsgId/binary, "/timestamp">>, integer_to_binary(erlang:system_time(second))),
        elmdb:put(DB, <<"messages/", MsgId/binary, "/read">>, <<"false">>),
        elmdb:put(DB, <<"messages/", MsgId/binary, "/priority">>, integer_to_binary(I rem 3))
    end, lists:seq(1, 1000)),
    MsgEnd = erlang:monotonic_time(millisecond),
    MsgTime = MsgEnd - MsgStart,
    io:format("Created 1000 messages in ~pms (~p messages/s)~n", 
              [MsgTime, 1000000 div MsgTime]),
    
    % Read messages (full message with all fields)
    io:format("~nReading messages...~n"),
    ReadStart = erlang:monotonic_time(millisecond),
    
    MessagesRead = lists:foldl(fun(I, Acc) ->
        MsgId = integer_to_binary(I),
        % Read all message fields like real workload
        {ok, _From} = elmdb:get(DB, <<"messages/", MsgId/binary, "/from">>),
        {ok, _To} = elmdb:get(DB, <<"messages/", MsgId/binary, "/to">>),
        {ok, _Subject} = elmdb:get(DB, <<"messages/", MsgId/binary, "/subject">>),
        {ok, _Body} = elmdb:get(DB, <<"messages/", MsgId/binary, "/body">>),
        {ok, _Timestamp} = elmdb:get(DB, <<"messages/", MsgId/binary, "/timestamp">>),
        {ok, _Read} = elmdb:get(DB, <<"messages/", MsgId/binary, "/read">>),
        {ok, _Priority} = elmdb:get(DB, <<"messages/", MsgId/binary, "/priority">>),
        % Also list message fields
        {ok, _Fields} = elmdb:list(DB, <<"messages/", MsgId/binary, "/">>),
        Acc + 1
    end, 0, lists:seq(1, 250)),
    
    ReadEnd = erlang:monotonic_time(millisecond),
    ReadTime = ReadEnd - ReadStart,
    io:format("Read ~p messages in ~pms (~p messages/s)~n", 
              [MessagesRead, ReadTime, (MessagesRead * 1000) div ReadTime]),
    
    % Mixed workload test
    io:format("~nMixed workload (reads + writes + lists)...~n"),
    MixedStart = erlang:monotonic_time(millisecond),
    
    lists:foreach(fun(I) ->
        % Write new message
        NewMsgId = integer_to_binary(10000 + I),
        elmdb:put(DB, <<"messages/", NewMsgId/binary, "/from">>, <<"user", (integer_to_binary(I))/binary>>),
        elmdb:put(DB, <<"messages/", NewMsgId/binary, "/subject">>, <<"New message">>),
        
        % Read existing message
        OldMsgId = integer_to_binary(I),
        {ok, _} = elmdb:get(DB, <<"messages/", OldMsgId/binary, "/subject">>),
        
        % List group
        GroupId = integer_to_binary(I * 100),
        {ok, _} = elmdb:list(DB, <<"groups/", GroupId/binary, "/">>)
    end, lists:seq(1, 1000)),
    
    MixedEnd = erlang:monotonic_time(millisecond),
    MixedTime = MixedEnd - MixedStart,
    io:format("Mixed operations (3000 ops) in ~pms (~p ops/s)~n", 
              [MixedTime, 3000000 div MixedTime]),
    
    io:format("~nSummary:~n"),
    io:format("  Group operations: ~p groups/s~n", [(TotalListed * 1000) div ListTime]),
    io:format("  Message reads: ~p messages/s~n", [(MessagesRead * 1000) div ReadTime]),
    io:format("  Mixed workload: ~p ops/s~n", [3000000 div MixedTime]),
    
    elmdb:env_close(Env),
    io:format("~nTest completed.~n").