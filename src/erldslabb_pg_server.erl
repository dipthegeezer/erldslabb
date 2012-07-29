-module(erldslabb_pg_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

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

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([any()]) -> {'ok', #state{conn::any()}}.
init([Hostname,Port,Database,Username,Password]) ->
    {ok, Conn} = pgsql:connect(
                   Hostname,
                   Username,
                   Password,
                   [{database, Database},
                    {port, Port}]
                  ),
    {ok, #state{conn=Conn}}.

%handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
%    {reply, pgsql:squery(Conn, Sql), State};
%handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
%    {reply, pgsql:equery(Conn, Stmt, Params), State};
handle_call({add_user, Params}, _From, #state{conn=Conn}=State) ->
    {reply,
     pgsql:equery(
              Conn,
              "INSERT INTO users (email, username, password, salt, date_of_birth)"
              ++ "VALUES($1, $2, $3, $4, $5)", Params
             ),
     State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_, #state{conn::any()}) -> 'ok'.
terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_timestamp() ->
    {Megaseconds,Seconds,Microseconds} = erlang:now(),
    (Megaseconds*1000000+Seconds)*1000000+Microseconds.

hash_password(Salt, Password) ->
    Cxt = crypto:hmac_init(sha,Salt),
    Cxt2 = crypto:hmac_update(Cxt,Password),
    Mac = crypto:hmac_final(Cxt2),
    hexstring(Mac).

hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).
