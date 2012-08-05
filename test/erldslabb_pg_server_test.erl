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
         ?_assertMatch({ok,[[{<<"id">>,_Id},
                             {<<"email">>,<<"arse@hole.com">>},
                             {<<"username">>,<<"chimp">>},
                             {<<"password">>,_P},
                             {<<"salt">>,_S},
                             {<<"date_of_birth">>,{1978,12,21}}]]},
                       Return)
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
         {ok,Ret} = gen_server:call(Pid,{add_user,Params}),
         Result = lists:nth(1, Ret),
         Id = proplists:get_value(<<"id">>, Result),
         P = proplists:get_value(<<"password">>, Result),
         S = proplists:get_value(<<"salt">>, Result),
         Return = gen_server:call(Pid,{get_user,Id}),
         ?_assertMatch(
            {ok,[
                 [{<<"id">>,Id},
                  {<<"email">>,<<"arse@hole.com">>},
                  {<<"username">>,<<"chimp">>},
                  {<<"password">>,P},
                  {<<"salt">>,S},
                  {<<"date_of_birth">>,{1978,12,21}}]
                ]
            },
            Return)
     end}.

delete_user_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         Params = [{<<"username">>,<<"chimp">>},
                   {<<"email">>,<<"arse@hole.com">>},
                   {<<"password">>,<<"finger">>},
                   {<<"date_of_birth">>,{1978,12,21}}],
         {ok,Ret} = gen_server:call(Pid,{add_user,Params}),
         Result = lists:nth(1, Ret),
         Id = proplists:get_value(<<"id">>, Result),
         Return = gen_server:call(Pid,{delete_user,Id}),
         ?_assertMatch({ok,1}, Return)
     end}.
