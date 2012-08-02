-module(erldslabb_pg_server_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok,Pid} = erldslabb_pg_server:start_link(
                 ["localhost",5432,"dslabb","dslabb","dslabb"]
                ),
    %% quick for now will deal with setup/teardown later
    {ok,_,_} = gen_server:call(Pid,{squery,"TRUNCATE TABLE users"}),
    Pid.

cleanup(Pid) ->
    gen_server:call(Pid,stop).

add_user_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         Params = [{<<"username">>,<<"chimp">>},
                   {<<"email">>,<<"arse@hole.com">>},
                   {<<"password">>,<<"finger">>},
                   {<<"date_of_birth">>,{1978,12,21}}],
         Return = gen_server:call(Pid,{add_user,Params}),
         ?_assertEqual({ok,1},Return)
     end}.

get_user_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         Params = [{<<"username">>,<<"chimp">>},
                   {<<"email">>,<<"arse@hole.com">>},
                   {<<"password">>,<<"finger">>},
                   {<<"date_of_birth">>,{1978,12,21}}],
         Ret = gen_server:call(Pid,{add_user,Params}),
         ?_assertMatch({ok,1},Ret),
         Return = gen_server:call(Pid,{get_user,<<"chimp">>}),
         ?_assertMatch(
            {ok,_Something,
             [{_Id,<<"arse@hole.com">>,
               <<"chimp">>,
               _Password,
               _Salt,{1978,12,21}}]},
            Return)
     end}.

