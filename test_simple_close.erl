#\!/usr/bin/env escript
%%\! -pa _build/default/lib/elmdb/ebin

main(_) ->
    Path = "/tmp/test_close",
    os:cmd("rm -rf " ++ Path),
    filelib:ensure_dir(Path ++ "/"),
    
    {ok, Env} = elmdb:env_open(list_to_binary(Path), [{map_size, 10485760}]),
    {ok, DB} = elmdb:db_open(Env, [create]),
    ok = elmdb:put(DB, <<"key">>, <<"value">>),
    
    % Test 1: Try closing env with active DB (should fail)
    io:format("Test 1: Close env with active DB: "),
    case elmdb:env_close(Env) of
        {error, _, Msg} -> io:format("Failed as expected: ~s~n", [Msg]);
        ok -> io:format("ERROR: Should have failed\!~n")
    end,
    
    % Test 2: Close DB then env (should work)
    io:format("Test 2: Close DB then env: "),
    ok = elmdb:db_close(DB),
    ok = elmdb:env_close(Env),
    io:format("Success\!~n"),
    
    % Test 3: Force close
    io:format("Test 3: Force close with active DB: "),
    {ok, Env2} = elmdb:env_open(<<"/tmp/test_close2">>, [{map_size, 10485760}]),
    {ok, DB2} = elmdb:db_open(Env2, [create]),
    case elmdb:env_force_close(Env2) of
        {ok, _} -> io:format("Success with warning~n");
        ok -> io:format("Success~n")
    end.
