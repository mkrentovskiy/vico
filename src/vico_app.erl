%% @doc Callbacks for the vico application.

-module(vico_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for vico.
start(_Type, _StartArgs) ->
    vico_deps:ensure(),
    vico_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for vico.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
