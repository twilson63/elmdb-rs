%% Common Test specification file for elmdb tests

{alias, elmdb_tests, "./test/"}.

{suites, elmdb_tests, [
    elmdb_basic_SUITE,
    elmdb_list_SUITE, 
    elmdb_error_SUITE,
    elmdb_perf_SUITE,
    elmdb_benchmark
]}.

{logdir, "./logs/"}.

{include, ["../src/"]}.

{cover, "./cover.spec"}.

%% Test configuration
{config, [
    {basic_timeout, 30000},      % 30 seconds for basic tests
    {performance_timeout, 600000} % 10 minutes for performance tests
]}.