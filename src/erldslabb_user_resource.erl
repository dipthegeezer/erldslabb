%% @author Dipesh Patel <dipthegeezer.opensource@gmail.com>
%% @copyright 2012 author.
%% @doc Resource for User

-module(erldslabb_user_resource).
-export([init/1, to_json/2, content_types_provided/2, allowed_methods/2]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET','POST','DELETE'], ReqData, Context}.

to_json(ReqData, State) ->
    Args = wrq:path_tokens(ReqData),
    Method = wrq:method(ReqData),
    io:fwrite("Args ~p Params ~p ~n",[Args, Method]),
    Result = execute_action(Args, ReqData),
    {Result, ReqData, State}.

execute_get(["find"], _) ->
    io:fwrite("Args ~p Params ~n",["find"]),
    "{moo:bar}";
execute_get([Id], _) ->
    io:fwrite("Args ~p Params ~n",[Id]),
    "{id:Id}".

process_post(ReqData, State) ->
    Args = wrq:path_tokens(ReqData),
    Method = wrq:method(ReqData),
    io:fwrite("Args ~p Params ~p ~n",[Args, Method]),
    Result = execute_post(Args, ReqData),
    {Result, ReqData, State}.

execute_post(["create"], _) ->
    io:fwrite("Args ~p Params ~n",["create"]),
    "{foo:bar}";


delete_resource(ReqData, State) ->
    Args = wrq:path_tokens(ReqData),
    io:fwrite("Args ~p Params ~n",[Args]),
    {true, ReqData ,State}.

