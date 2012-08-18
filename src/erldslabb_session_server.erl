-module(erldslabb_session_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    random:seed(now()),
    {ok, Args}.

handle_call({find_or_create_session, SessionId}, _From, State) ->
    case eredis_cmd(["EXISTS", SessionId]) of
        {ok, <<"1">>} -> {reply, {ok, SessionId}, State};
        {ok, <<"0">>} -> NewSessionId = get_session_id(),
                         case eredis_cmd(["SETEX", NewSessionId, 800, "{}"]) of
                             {error, Error} ->
                                 {reply, {error, Error}, State};
                             {ok,<<"OK">>} ->
                                 {reply, {ok, NewSessionId}, State}
                         end
    end;
handle_call({get_session_data, SessionId}, _From, State) ->
    case get_session_data(SessionId) of
        {ok, Data} -> {reply, {ok, Data}, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;

handle_call({get_session_data, SessionId, Key}, _From, State) ->
    case get_session_data(SessionId) of
        {ok, Data} -> Value = proplists:get_value(Key, Data),
                      {reply, {ok, Value}, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call({set_session_data, SessionId, Key, Value}, _From, State) ->
    case get_session_data(SessionId) of
        {ok, Data} -> NewData = proplists:delete(Key,Data)++[{Key,Value}],
                      JsonDoc = erldslabb_util:to_json_binary(NewData),
                      case eredis_cmd(["SETEX", SessionId, 800, JsonDoc]) of
                          {error, Error} ->
                              {reply, {error, Error}, State};
                          {ok,<<"OK">>} ->
                              {reply, ok, State}
                      end;
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call({delete_session, SessionId}, _From, State) ->
    case eredis_cmd(["DEL", SessionId]) of
        {ok, <<"1">>} -> {reply, ok, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call({remove_session_data, SessionId, Key}, _From, State) ->
    {ok, Data} = get_session_data(SessionId),
    NewData = proplists:delete(Key,Data),
    JsonDoc = erldslabb_util:to_json_binary(NewData),
    case eredis_cmd(["SETEX", SessionId, 800, JsonDoc]) of
        {error, Error} ->
            {reply, {error, Error}, State};
        {ok,<<"OK">>} ->
            {reply, ok, State}
    end;
handle_call({has_expired, SessionId}, _From, State) ->
    case eredis_cmd(["EXISTS", SessionId]) of
        {ok, <<"1">>} -> {reply, false, State};
        {ok, <<"0">>} -> {reply, true, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_session_id() ->
    Data = crypto:sha(crypto:strong_rand_bytes(4096)),
    erldslabb_util:hexstring(Data).

eredis_cmd(Args) ->
    poolboy:transaction( dick, fun(Worker) -> eredis:q(Worker, Args) end ).

get_session_data(SessionId) ->
    case eredis_cmd(["GET", SessionId]) of
        {ok, JsonDoc} -> Prop = erldslabb_util:to_proplist(JsonDoc),
                         {ok, Prop};
        {error, Error} -> {error, Error}
    end.

%% ------------------------------------------------------------------
%% Internal Function Tests
%% ------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

get_session_id_test() ->
    Id = get_session_id(),
    ?assert(string:len(Id)=:=40).

-endif.
