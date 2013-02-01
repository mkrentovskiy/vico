-module(actions).
-export([start/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(sockjs),
	ok = application:start(cowboy),
	ok = application:start(actions).
