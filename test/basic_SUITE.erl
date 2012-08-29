-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([run_erldslabb_util/1,
         run_erldslabb_pg_server/1,
         run_erldslabb_session_server/1,
         init_per_suite/1
        ]).

all() -> [run_erldslabb_util, run_erldslabb_pg_server, run_erldslabb_session_server].

init_per_suite(InitConfigData) ->
    io:fwrite(standard_error,"config ~p~n",[InitConfigData]),
    InitConfigData.

run_erldslabb_util(_Config) ->
    ok = eunit:test(erldslabb_util).

run_erldslabb_pg_server(_Config) ->
    ok = eunit:test(erldslabb_pg_server).

run_erldslabb_session_server(_Config) ->
    ok = eunit:test(erldslabb_session_server).

