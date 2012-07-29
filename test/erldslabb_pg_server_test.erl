-module(erldslabb_pg_server_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok,Pid} = erldslabb_pg_server:start_link(
                 ["localhost",5432,"dslabb","dslabb","dslabb"]
               ), Pid.

cleanup(Pid) ->
    gen_server:cast(Pid,stop).

query_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"inc by 0",
       ?_test(?assertEqual(1, 1))}]
    }.
