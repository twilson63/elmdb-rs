%%%-------------------------------------------------------------------
%%% @doc
%%% List operations tests for elmdb NIF
%%% Tests hierarchical key structure with list/2 function
%%% @end
%%%-------------------------------------------------------------------
-module(elmdb_list_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% Common Test callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    ok = application:start(elmdb),
    Config.

end_per_suite(_Config) ->
    application:stop(elmdb),
    ok.

init_per_testcase(TestCase, Config) ->
    % Create unique test directory for each test case
    TestDir = filename:join([?config(priv_dir, Config), 
                            atom_to_list(TestCase)]),
    ok = filelib:ensure_dir(TestDir ++ "/"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    % Clean up test directory
    TestDir = ?config(test_dir, Config),
    case file:del_dir_r(TestDir) of
        ok -> ok;
        {error, enoent} -> ok;
        Error -> ct:pal("Failed to clean up ~p: ~p", [TestDir, Error])
    end,
    ok.

all() ->
    [
        test_list_empty_database,
        test_list_nonexistent_prefix,
        test_list_single_level,
        test_list_hierarchical_keys,
        test_list_direct_children_only,
        test_list_prefix_edge_cases,
        test_list_with_similar_prefixes,
        test_list_binary_prefixes,
        test_list_root_level,
        test_list_deep_hierarchy,
        test_list_partial_matches
    ].

%%====================================================================
%% Helper functions
%%====================================================================

setup_database(Config) ->
    TestDir = ?config(test_dir, Config),
    {ok, Env} = elmdb:env_open(TestDir, []),
    {ok, DB} = elmdb:db_open(Env, [create]),
    {Env, DB}.

populate_hierarchical_data(DB) ->
    % Create a hierarchical structure like a file system
    TestData = [
        {<<"users">>, <<"users_metadata">>},
        {<<"users/alice">>, <<"alice_data">>},
        {<<"users/alice/profile">>, <<"alice_profile">>},
        {<<"users/alice/settings">>, <<"alice_settings">>},
        {<<"users/alice/documents/doc1">>, <<"alice_doc1">>},
        {<<"users/alice/documents/doc2">>, <<"alice_doc2">>},
        {<<"users/bob">>, <<"bob_data">>},
        {<<"users/bob/profile">>, <<"bob_profile">>},
        {<<"users/bob/settings">>, <<"bob_settings">>},
        {<<"users/charlie">>, <<"charlie_data">>},
        {<<"groups">>, <<"groups_metadata">>},
        {<<"groups/admin">>, <<"admin_group">>},
        {<<"groups/users">>, <<"users_group">>},
        {<<"config">>, <<"config_data">>},
        {<<"config/database">>, <<"db_config">>},
        {<<"config/network">>, <<"net_config">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    TestData.

%%====================================================================
%% List Operations Tests
%%====================================================================

test_list_empty_database(Config) ->
    {Env, DB} = setup_database(Config),
    
    % List from empty database should return not_found
    Result = elmdb:list(DB, <<"any_prefix">>),
    not_found = Result,
    
    ok = elmdb:env_close(Env),
    ok.

test_list_nonexistent_prefix(Config) ->
    {Env, DB} = setup_database(Config),
    populate_hierarchical_data(DB),
    
    % List with non-existent prefix should return not_found
    Result = elmdb:list(DB, <<"nonexistent">>),
    not_found = Result,
    
    ok = elmdb:env_close(Env),
    ok.

test_list_single_level(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Create simple flat keys
    TestData = [
        {<<"item1">>, <<"value1">>},
        {<<"item2">>, <<"value2">>},
        {<<"item3">>, <<"value3">>},
        {<<"other">>, <<"other_value">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    % List with "item" prefix should return item1, item2, item3
    {ok, Results} = elmdb:list(DB, <<"item">>),
    
    % Results should be sorted and contain all items with prefix
    ExpectedKeys = [<<"item1">>, <<"item2">>, <<"item3">>],
    SortedResults = lists:sort(Results),
    SortedExpected = lists:sort(ExpectedKeys),
    SortedExpected = SortedResults,
    
    ok = elmdb:env_close(Env),
    ok.

test_list_hierarchical_keys(Config) ->
    {Env, DB} = setup_database(Config),
    populate_hierarchical_data(DB),
    
    % List all keys starting with "users/"
    {ok, UserResults} = elmdb:list(DB, <<"users/">>),
    
    % Should include all user-related keys
    ExpectedUserKeys = [
        <<"users/alice">>,
        <<"users/alice/profile">>,
        <<"users/alice/settings">>,
        <<"users/alice/documents/doc1">>,
        <<"users/alice/documents/doc2">>,
        <<"users/bob">>,
        <<"users/bob/profile">>,
        <<"users/bob/settings">>,
        <<"users/charlie">>
    ],
    
    % Verify all expected keys are present
    lists:foreach(fun(ExpectedKey) ->
        true = lists:member(ExpectedKey, UserResults)
    end, ExpectedUserKeys),
    
    ok = elmdb:env_close(Env),
    ok.

test_list_direct_children_only(Config) ->
    {Env, DB} = setup_database(Config),
    populate_hierarchical_data(DB),
    
    % This test depends on the implementation of list/2
    % If it returns only direct children vs all descendants with prefix
    
    % List root level items
    {ok, RootResults} = elmdb:list(DB, <<"">>),
    
    % Should include top-level keys
    RootLevelKeys = [<<"users">>, <<"groups">>, <<"config">>],
    lists:foreach(fun(Key) ->
        true = lists:member(Key, RootResults)
    end, RootLevelKeys),
    
    ok = elmdb:env_close(Env),
    ok.

test_list_prefix_edge_cases(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Test edge cases with prefixes
    TestData = [
        {<<"a">>, <<"value_a">>},
        {<<"aa">>, <<"value_aa">>},
        {<<"aaa">>, <<"value_aaa">>},
        {<<"ab">>, <<"value_ab">>},
        {<<"abc">>, <<"value_abc">>},
        {<<"b">>, <<"value_b">>},
        {<<"ba">>, <<"value_ba">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    % Test exact match prefix
    {ok, AResults} = elmdb:list(DB, <<"a">>),
    ExpectedAKeys = [<<"a">>, <<"aa">>, <<"aaa">>, <<"ab">>, <<"abc">>],
    lists:foreach(fun(Key) ->
        true = lists:member(Key, AResults)
    end, ExpectedAKeys),
    
    % Test longer prefix
    {ok, AAResults} = elmdb:list(DB, <<"aa">>),
    ExpectedAAKeys = [<<"aa">>, <<"aaa">>],
    lists:foreach(fun(Key) ->
        true = lists:member(Key, AAResults)
    end, ExpectedAAKeys),
    
    ok = elmdb:env_close(Env),
    ok.

test_list_with_similar_prefixes(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Test with similar but distinct prefixes
    TestData = [
        {<<"user">>, <<"user_data">>},
        {<<"user1">>, <<"user1_data">>},
        {<<"user12">>, <<"user12_data">>},
        {<<"user123">>, <<"user123_data">>},
        {<<"user2">>, <<"user2_data">>},
        {<<"user_info">>, <<"user_info_data">>},
        {<<"users">>, <<"users_data">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    % List with "user1" prefix
    {ok, User1Results} = elmdb:list(DB, <<"user1">>),
    ExpectedUser1Keys = [<<"user1">>, <<"user12">>, <<"user123">>],
    lists:foreach(fun(Key) ->
        true = lists:member(Key, User1Results)
    end, ExpectedUser1Keys),
    
    % Ensure user2 is not included
    false = lists:member(<<"user2">>, User1Results),
    
    ok = elmdb:env_close(Env),
    ok.

test_list_binary_prefixes(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Test with binary data as prefixes
    TestData = [
        {<<1,2,3>>, <<"binary_value1">>},
        {<<1,2,3,4>>, <<"binary_value2">>},
        {<<1,2,3,4,5>>, <<"binary_value3">>},
        {<<1,2,4>>, <<"different_binary">>},
        {<<1,3>>, <<"another_binary">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    % List with binary prefix <<1,2,3>>
    {ok, BinaryResults} = elmdb:list(DB, <<1,2,3>>),
    ExpectedBinaryKeys = [<<1,2,3>>, <<1,2,3,4>>, <<1,2,3,4,5>>],
    lists:foreach(fun(Key) ->
        true = lists:member(Key, BinaryResults)
    end, ExpectedBinaryKeys),
    
    % Ensure other keys are not included
    false = lists:member(<<1,2,4>>, BinaryResults),
    false = lists:member(<<1,3>>, BinaryResults),
    
    ok = elmdb:env_close(Env),
    ok.

test_list_root_level(Config) ->
    {Env, DB} = setup_database(Config),
    populate_hierarchical_data(DB),
    
    % List all keys (empty prefix)
    {ok, AllResults} = elmdb:list(DB, <<"">>),
    
    % Should include all keys in the database
    ExpectedKeys = [
        <<"users">>, <<"users/alice">>, <<"users/alice/profile">>,
        <<"users/alice/settings">>, <<"users/alice/documents/doc1">>,
        <<"users/alice/documents/doc2">>, <<"users/bob">>,
        <<"users/bob/profile">>, <<"users/bob/settings">>,
        <<"users/charlie">>, <<"groups">>, <<"groups/admin">>,
        <<"groups/users">>, <<"config">>, <<"config/database">>,
        <<"config/network">>
    ],
    
    % Verify all keys are present
    lists:foreach(fun(Key) ->
        true = lists:member(Key, AllResults)
    end, ExpectedKeys),
    
    ok = elmdb:env_close(Env),
    ok.

test_list_deep_hierarchy(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Create a deep hierarchy
    DeepData = [
        {<<"a/b/c/d/e/f/g">>, <<"deep_value1">>},
        {<<"a/b/c/d/e/f/h">>, <<"deep_value2">>},
        {<<"a/b/c/d/e/i">>, <<"deep_value3">>},
        {<<"a/b/c/j">>, <<"deep_value4">>},
        {<<"a/b/k">>, <<"deep_value5">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, DeepData),
    
    % List with deep prefix
    {ok, DeepResults} = elmdb:list(DB, <<"a/b/c/d/e">>),
    ExpectedDeepKeys = [
        <<"a/b/c/d/e/f/g">>,
        <<"a/b/c/d/e/f/h">>,
        <<"a/b/c/d/e/i">>
    ],
    lists:foreach(fun(Key) ->
        true = lists:member(Key, DeepResults)
    end, ExpectedDeepKeys),
    
    ok = elmdb:env_close(Env),
    ok.

test_list_partial_matches(Config) ->
    {Env, DB} = setup_database(Config),
    
    % Test partial key matches
    TestData = [
        {<<"test">>, <<"test_exact">>},
        {<<"testing">>, <<"test_ing">>},
        {<<"tester">>, <<"test_er">>},
        {<<"tested">>, <<"test_ed">>},
        {<<"te">>, <<"te_short">>},
        {<<"tests">>, <<"test_s">>}
    ],
    
    lists:foreach(fun({Key, Value}) ->
        ok = elmdb:put(DB, Key, Value)
    end, TestData),
    
    % List with "test" prefix (should match all starting with "test")
    {ok, TestResults} = elmdb:list(DB, <<"test">>),
    ExpectedTestKeys = [<<"test">>, <<"testing">>, <<"tester">>, <<"tested">>, <<"tests">>],
    lists:foreach(fun(Key) ->
        true = lists:member(Key, TestResults)
    end, ExpectedTestKeys),
    
    % "te" should not be included as it doesn't start with "test"
    false = lists:member(<<"te">>, TestResults),
    
    ok = elmdb:env_close(Env),
    ok.