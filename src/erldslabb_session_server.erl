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

handle_call({get_session, SessionId}, _From, State) ->
    case poolboy:transaction(
           dick, fun(Worker) ->
                     eredis:q(Worker,["EXISTS", SessionID])
                 end ) of
        {ok, <<"1">>} -> {reply, {ok, SessionID}, State};
        {ok, <<"0">>} -> NewSessionID = get_session_id(),
                         case poolboy:transaction(
                                dick, fun(Worker) ->
                                          eredis:q(Worker,["SETX", SessionID, 800, ""])
                                 end ) of
                             {error, Error} ->
                                 {reply, {error, Error}, State};
                             {ok,<<"OK">>} ->
                                 {reply, {ok, NewSessionID}, State}
                             end
    end;
handle_call({get_session_data, SessionId}, _From, State) ->
    {reply, ok, State};
handle_call({get_session_data, SessionId, Key}, _From, State) ->
    {reply, ok, State};
handle_call({set_session_data, SessionId, Key, Value}, _From, State) ->
    {reply, ok, State};
handle_call({delete_session, SessionID}, _From, State) ->
    {reply, ok, State};
handle_call({remove_session_data, Sessionid, Key}, _From, State) ->
    {reply, ok, State}.

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

get_session_id()->
    Data = crypto:sha(crypto:strong_rand_bytes(4096)),
    erldslabb_util:hexstring(Data).
