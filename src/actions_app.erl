-module(actions_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Events = sockjs_handler:init_state(<<"/a">>, fun events/3, state, [{response_limit, 1024}]),
	VRoutes = [
		{"/", cowboy_static, [
				{directory,  <<"/opt/vico/priv/www">>},
				{file, <<"index.html">>},
				{mimetypes, {fun mimetypes:path_to_mimes/2, default}} 
			]},
		{<<"/a/[...]">>, sockjs_cowboy_handler, Events},
		{<<"/[...]">>, cowboy_static, [
				{directory,  <<"/opt/vico/priv/www">>},
				{mimetypes, {fun mimetypes:path_to_mimes/2, default}} 
			]},
		{'_', notfound_handler, []}],
	Routes = [{'_',  VRoutes}], 
	Dispatch = cowboy_router:compile(Routes),
	cowboy:start_http(webapp_http_listener, 5, 
			[{port, 8080}],
			[{env, [{dispatch, Dispatch}]}
		]),
	actions_sup:start_link().

stop(_State) ->
	ok.

events(Con, init, _) -> State = room:connect(Con), {ok, State};
events(Con, {recv, Msg}, State) -> NewState = room:msg(Con, Msg, State), {ok, NewState};
events(Con, closed, State) -> NewState = room:close(Con, State), {ok, NewState}.