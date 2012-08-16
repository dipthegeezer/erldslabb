-module(erldslabb_session_server_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok,Pid} = erldslabb_session_server:start_link(),
    %% quick for now will deal with setup/teardown later
    Pid.

cleanup(Pid) ->
    gen_server:call(Pid,stop).

find_or_create_session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         SessionId = gen_server:call(Pid,{find_or_create_session,""}),
         ?_assert(string:len(SessionId)=:=40)
     end}.
