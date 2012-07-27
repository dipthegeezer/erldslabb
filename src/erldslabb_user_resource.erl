%% @author Dipesh Patel <dipthegeezer.opensource@gmail.com>
%% @copyright 2012 author.
%% @doc Resource for User

-module(erldslabb_user_resource).
-export([
         init/1,
         content_types_accepted/2,
         content_types_provided/2,
         allowed_methods/2,
         resource_exists/2,
         allow_missing_post/2,
         process_post/2,
         delete_resource/2,
         finish_request/2,
         to_json/2,
         from_json/2
        ]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET','POST','DELETE','HEAD','PUT'], ReqData, Context}.

finish_request(ReqData, Context) ->
    {true, ReqData, Context}.

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

process_post(ReqData, Context) ->
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    io:fwrite("Args ~p post ~n",[Doc]),
    {true, ReqData, Context}.

delete_resource(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    io:fwrite("Delete ~p ~n",[Id]),
    {true, ReqData, Context}.
    %case prp_schema:delete_paper(Id) of
     %   ok    -> {true, RD, Ctx};
     %   _Else -> {false, RD, Ctx}
    %end.

resource_exists(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    io:fwrite("Exists ~p ~n",[Id]),
    {true, ReqData, Context}.

to_json(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    % to fill in get etc.
    Resp = "{id:" ++ Id ++ "}",
    {Resp, ReqData, Context}.

from_json(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    io:fwrite("Args ~p post ~n",[Doc]),
    Resp2 = wrq:set_resp_body("{put_id:" ++ Id ++ "}", ReqData),
    io:fwrite("Putting ~p ~n",[Id]),
    {true, Resp2, Context}.
