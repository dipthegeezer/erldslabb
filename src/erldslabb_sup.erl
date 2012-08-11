%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the erldslabb application.

-module(erldslabb_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, App} = application:get_application(?MODULE),
    {ok, Dispatch} = file:consult(filename:join([code:priv_dir(App),
                                                 "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    {ok, Pools} = application:get_env(erldslabb, pools),
    PoolSpecs = lists:map(
       fun(Pool) ->
           PoolArgs = proplists:get_value(poolargs,Pool),
           {_, Name} = proplists:get_value(name,PoolArgs),
           WorkerArgs = proplists:get_value(workerargs,Pool),
           poolboy:child_spec(Name, PoolArgs, WorkerArgs)
       end, Pools),
    io:fwrite(standard_error,"SPECS ~p~n",[PoolSpecs]),
    Processes = [Web]++PoolSpecs,
    {ok, { {one_for_one, 10, 10}, Processes} }.
