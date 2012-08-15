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

-record(state, {conn}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({new_session, CookieSessionId}, _From, State) ->
    % Return session if exists from cookieSessionId
    % else generate session and return
    {reply, ok, State};
handle_call({get_session_data, Sid}, _From, State) ->
    {reply, ok, State};
handle_call({get_session_data, Sid, Key}, _From, State) ->
    {reply, ok, State};
handle_call({set_session_data, Sid, Key, Value}, _From, State) ->
    {reply, ok, State};
handle_call({delete_session, SessionID}, _From, State) ->
    {reply, ok, State};
handle_call({remove_session_data, Sid, Key}, _From, State) ->
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

