-module(erldslabb_session_server_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    %% quick for now will deal with setup/teardown later
    {ok,Pid2} = poolboy:start_link(
                  [{name,{local,dick}},
                   {worker_module,eredis},
                   {size,10},
                   {max_overflow,20} ],
                  []),
    {ok,Pid} = erldslabb_session_server:start_link(),
    {Pid,Pid2}.

cleanup({Pid,Pid2}) ->
    ok = gen_fsm:sync_send_all_state_event(Pid2, stop),
    ok = gen_server:call(Pid,stop),
    ok.

find_or_create_session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({H,_}) ->
         {ok,SessionId} = gen_server:call(H,{find_or_create_session,""}),
         {ok,SessionId} = gen_server:call(H,{find_or_create_session,SessionId}),
         ?_assert(string:len(SessionId)=:=40)
     end}.

get_set_session_data_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({H,_}) ->
         {ok,SessionId} = gen_server:call(H,{find_or_create_session,""}),
         Bar = <<"bar">>,
         ok = gen_server:call(H,{set_session_data,SessionId,<<"foo">>,Bar}),
         {ok,Bar} = gen_server:call(H,{get_session_data,SessionId,<<"foo">>}),
         {ok,Data} = gen_server:call(H,{get_session_data,SessionId}),
         ?_assertMatch([{<<"foo">>,<<"bar">>}],Data)
     end}.

del_remove_session_data_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({H,_}) ->
         {ok,SessionId} = gen_server:call(H,{find_or_create_session,""}),
         Bar = <<"bar">>,
         ok = gen_server:call(H,{set_session_data,SessionId,<<"foo">>,Bar}),
         {ok,Bar} = gen_server:call(H,{get_session_data,SessionId,<<"foo">>}),
         ok = gen_server:call(H,{remove_session_data,SessionId,<<"foo">>}),
         {ok,undefined} = gen_server:call(H,{get_session_data,SessionId,<<"foo">>}),
         ok = gen_server:call(H,{delete_session,SessionId}),
         {ok,NewSessionId} = gen_server:call(H,{find_or_create_session,SessionId}),
         ?_assert(SessionId=/=NewSessionId)
     end}.
