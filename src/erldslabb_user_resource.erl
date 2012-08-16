%% @copyright 2012 Dipesh Patel
%% @author Dipesh Patel<dipthegeezer.opensource@gmail.com>
%% @end

%% @doc Resource for creating editing a user
%% GET ==
%% - get user
%% HEAD ==
%% - get user
%% POST ==
%% - create user
%% PUT ==
%% - update user
%% DELETE ==
%% - delete user

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

-record(ctx, {user}).

init([]) -> {ok, #ctx{}}.

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
    FDate = erldslabb_util:json_date_format_for_epgsql(
      proplists:get_value(<<"date_of_birth">>, Doc)
    ),
    Doc2 = proplists:delete(<<"date_of_birth">>, Doc)
        ++[{<<"date_of_birth">>,FDate}],
    case pg_cmd({add_user,Doc2}) of
        {ok,[H|_]} ->
            {Resp, ReqData, Ctx} = to_json(ReqData,#ctx{user=H}),
            {true, wrq:set_resp_body(Resp, ReqData), Ctx};
        {error,Error} ->
            io:fwrite("Error ~p~n",[Error]),
            {false, ReqData, Context}
    end.

delete_resource(ReqData, Context=#ctx{user=User}) ->
    Id = proplists:get_value(<<"id">>, User),
    case pg_cmd({delete_user,Id}) of
        {ok,1} -> {true, ReqData, #ctx{}};
        {error,Error} -> io:fwrite("Error ~p~n",[Error]),
                         {false, ReqData, Context}
    end.

resource_exists(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' -> {true, ReqData, Context};
        _ -> Id = list_to_integer(wrq:path_info(id, ReqData)),
             case pg_cmd({get_user,Id}) of
                 {ok,[H|_]} ->
                     {true, ReqData, Context#ctx{user=H}};
                 {ok,[]} ->
                     {false, ReqData, Context}
             end
    end.

to_json(ReqData, Context=#ctx{user=User}) ->
    FDate = erldslabb_util:epgsql_date_format_for_json(
      proplists:get_value(<<"date_of_birth">>, User)
    ),
    Resp = iolist_to_binary(
             mochijson2:encode(
               {struct,
                proplists:delete(<<"date_of_birth">>, User)
                ++[{<<"date_of_birth">>,FDate}]
               }
              )
            ),
    {Resp, ReqData, Context}.

from_json(ReqData, Context) ->
    Id = list_to_integer(wrq:path_info(id, ReqData)),
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    % format date
    FDate = erldslabb_util:json_date_format_for_epgsql(
      proplists:get_value(<<"date_of_birth">>, Doc)
    ),
    Doc2 = proplists:delete(<<"date_of_birth">>, Doc)
        ++[{<<"date_of_birth">>,FDate}],
    case pg_cmd({update_user,Id,Doc2}) of
        {ok,[H|_]} ->
            {Resp, ReqData, Ctx} = to_json(ReqData,#ctx{user=H}),
            {true, wrq:set_resp_body(Resp, ReqData), Ctx};
        {error,Error} ->
            io:fwrite("Error ~p~n",[Error]),
            {false, ReqData, Context}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

pg_cmd(Args) ->
    poolboy:transaction( bruce, fun(Worker) ->
                                        gen_server:call(Worker, Args)
                                end ).
