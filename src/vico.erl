-module(vico).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the vico server.
start() ->
    vico_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mnesia),
    application:start(vico).

%% @spec stop() -> ok
%% @doc Stop the vico server.
stop() ->
    Res = application:stop(vico),
    application:stop(mnesia),    
    application:stop(crypto),
    Res.
