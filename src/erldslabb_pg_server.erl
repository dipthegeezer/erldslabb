%% @copyright 2012 Dipesh Patel
%% @author Dipesh Patel<dipthegeezer.opensource@gmail.com>
%% @end

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

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([Hostname,Port,Database,Username,Password]) ->
    {ok, Conn} = pgsql:connect(
                   Hostname,
                   Username,
                   Password,
                   [{database, Database},
                    {port, Port}]
                  ),
    {ok, #state{conn=Conn}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} | (terminate/2 is called)
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({add_user, Params}, _From, #state{conn=Conn}=State) ->
    Username = proplists:get_value(<<"username">>, Params),
    Email = proplists:get_value(<<"email">>, Params),
    Salt = list_to_binary(
        integer_to_list(erldslabb_util:get_timestamp())
    ),
    Password = erldslabb_util:hash_password(
        Salt,
        proplists:get_value(<<"password">>, Params)
    ),
    DOB = proplists:get_value(<<"date_of_birth">>, Params),
    %% for now bounce errors up
    case pgsql:equery(
           Conn,
           "INSERT INTO users "
           ++"(email, username, password, salt, date_of_birth)"
           ++ "VALUES($1, $2, $3, $4, $5) RETURNING *",
           [Email,Username,Password,Salt,DOB]
          ) of
        {ok, 1,Cols, Rows} ->
            {reply, {ok, map_to_list(Cols, Rows)}, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:squery(Conn, Sql), State};
handle_call({get_user, Id}, _From, #state{conn=Conn}=State) ->
    case pgsql:equery(Conn, "SELECT * FROM users where id=$1",[Id]) of
        {ok, Cols, Rows} ->
            {reply, {ok, map_to_list(Cols, Rows)}, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call({delete_user, Id}, _From, #state{conn=Conn}=State) ->
    % delete for now may switch to flag later.
    case pgsql:equery(Conn, "DELETE FROM users where id=$1",[Id]) of
        {ok, Count} ->
            {reply, {ok, Count}, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call({update_user, Id, Params}, _From, #state{conn=Conn}=State) ->
    {Sql,Args} = build_update_query(Id, Params),
    case pgsql:equery(Conn, Sql, Args) of
        {ok, 1,Cols, Rows} ->
            {reply, {ok, map_to_list(Cols, Rows)}, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @spec (iolist(), iolist()) -> iolist()
%% @doc Takes the column and row result given by
%% pgsql query and converts to a proplist where
%% column name is the key.
format_column_row_result(C,R) ->
    format_column_row_result(C,tuple_to_list(R),[]).

%% @spec (iolist(), iolist(), iolist()) -> iolist()
%% @see format_column_row_result(iolist(), iolist()) -> iolist()
format_column_row_result([],[],Results) ->
    lists:reverse(Results);
format_column_row_result([HeadCol|TailCol],[HeadRow|TailRow],Results) ->
    format_column_row_result(TailCol,TailRow,[{element(2,HeadCol),HeadRow}|Results]).

%% @spec (iolist(), iolist()) -> iolist()
%% @doc take the row and column results and return list of proplists
%% one for each row, with the column name as key.
map_to_list(Col,Rows)->
    lists:map(
      fun (Row) -> format_column_row_result(Col,Row) end,
      Rows).

build_update_query(Id,Params) ->
    Keys = proplists:get_keys(Params),
    Values = [proplists:get_value(K, Params) || K <- Keys],
    {Updates,Count} = build_set_clause(Keys,1,[]),
    { "UPDATE users SET "++ string:join(Updates, ", ")++ " WHERE id = \$"++integer_to_list(Count)
      ++ " RETURNING *",
      lists:append(Values,[Id])
    }.

build_set_clause([],Count,Updates)->
    {lists:reverse(Updates),Count};
build_set_clause([H|T],Count,Updates) ->
    build_set_clause(T,Count+1,[format_field(H) ++ " = \$" ++ integer_to_list(Count)|Updates]).

format_field(Field) when is_binary(Field) ->
    binary_to_list(Field);
format_field(Field) ->
    Field.

%%
%% Tests for Internal functions
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

format_column_row_result_test() ->
    Col = [{column,<<"id">>,int4,4,-1,1},
           {column,<<"email">>,text,-1,-1,1},
           {column,<<"username">>,text,-1,-1,1},
           {column,<<"password">>,text,-1,-1,1},
           {column,<<"salt">>,text,-1,-1,1},
           {column,<<"date_of_birth">>,date,4,-1,1}],
    Row = {55,<<"arse@hole.com">>,<<"chimp">>,
           <<"5pNpjaMyf0AJCHX5TGsPJM7l+k4=">>,
           <<"1343916294283580">>,{1978,12,21}},
    Result = format_column_row_result(Col,Row),
    ?assertMatch(
       [{<<"id">>,55},
        {<<"email">>,<<"arse@hole.com">>},
        {<<"username">>,<<"chimp">>},
        {<<"password">>,<<"5pNpjaMyf0AJCHX5TGsPJM7l+k4=">>},
        {<<"salt">>,<<"1343916294283580">>},
        {<<"date_of_birth">>,{1978,12,21}}],
       Result).

map_to_list_test() ->
    Col = [{column,<<"id">>,int4,4,-1,1},
           {column,<<"email">>,text,-1,-1,1},
           {column,<<"username">>,text,-1,-1,1},
           {column,<<"password">>,text,-1,-1,1},
           {column,<<"salt">>,text,-1,-1,1},
           {column,<<"date_of_birth">>,date,4,-1,1}],
    Rows = [{55,<<"arse@hole.com">>,<<"chimp">>,
           <<"5pNpjaMyf0AJCHX5TGsPJM7l+k4=">>,
           <<"1343916294283580">>,{1978,12,21}},
           {54,<<"dick@head.com">>,<<"chimpee">>,
           <<"5pNpjaMyf0AJCHX5TGsPJM7l+k4=">>,
           <<"1343916294283580">>,{1978,12,21}}],
    Result = map_to_list(Col,Rows),
    ?assertMatch(
       [[{<<"id">>,55},
        {<<"email">>,<<"arse@hole.com">>},
        {<<"username">>,<<"chimp">>},
        {<<"password">>,<<"5pNpjaMyf0AJCHX5TGsPJM7l+k4=">>},
        {<<"salt">>,<<"1343916294283580">>},
        {<<"date_of_birth">>,{1978,12,21}}],
        _Two
       ],
       Result).

build_update_query_test() ->
    Q = build_update_query( 12,
                            [{<<"email">>,<<"arse@hole.com">>},
                             {<<"username">>,<<"chimp">>}]),
    ?assertMatch({"UPDATE users SET username = $1, email = $2 WHERE id = $3 RETURNING *",
                  [<<"chimp">>,<<"arse@hole.com">>,12]},Q).

-endif.
