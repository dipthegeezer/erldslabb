%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erldslabb application.

-module(erldslabb_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erldslabb.
start(_Type, _StartArgs) ->
    erldslabb_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erldslabb.
stop(_State) ->
    ok.
