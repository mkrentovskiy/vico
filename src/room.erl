-module(room).
-behaviour(gen_server).

-include("room.hrl").

-export([connect/1, msg/3, close/2]).
-export([add/1, enter/2, pub/2, stop/2, chat/3, rm/2, send_msg/2, send_broadcast/2]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(rstate, { users = [], nicks = [], chat = [], video = [] }).
-record(ustate, { nick = undefine, id = undefine, stream = undefine }).

%
% SockJS events
%

connect(_Con) -> undefine.

msg(Con, <<"I">>, undefine) -> add(Con);
msg(Con, <<"L:", Nick/binary>>, undefine) ->
	case enter(Con, Nick) of
		{ok, UState} -> UState;
		error -> undefine
	end;
msg(Con, <<"P">>, S) when S =/= undefine -> pub(Con, S);
msg(Con, <<"S">>, S) when S =/= undefine ->	stop(Con, S);
msg(Con, <<"C:", Msg/binary>>, S) when S =/= undefine -> chat(Con, S, Msg);	
msg(_, _, S) -> S.

close(Con, S) -> rm(Con, S).

%
% genserver wrappes
%

add(Con) -> gen_server:call(?MODULE, {add, Con}).
enter(Con, Nick) -> gen_server:call(?MODULE, {enter, Con, Nick}).
pub(Con, S) -> gen_server:call(?MODULE, {pub, Con, S}).
stop(Con, S) -> gen_server:call(?MODULE, {stop, Con, S}).
chat(Con, S, Msg) -> gen_server:call(?MODULE, {chat, Con, S, Msg}).
rm(Con, S) -> gen_server:call(?MODULE, {rm, Con, S}).

%
% genserver
%

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #rstate{}}.

handle_call({add, Con}, _From, State) -> 
	NewState = State#rstate{ users = State#rstate.users ++ [Con] }, 
	proc_lib:spawn(fun() -> user_init(Con, NewState) end),
	{reply, undefine, NewState};

handle_call({enter, Con, Nick}, _From, State) -> 
	case lists:any(fun({N, _}) -> N == Nick end, State#rstate.nicks) of
		true ->
			send_msg(Con, lger([])),
			{reply, error, State};
		false ->
			Id = token(),
			Stream = token(),
			UState = #ustate{ nick = Nick, id = Id, stream = Stream },
			send_msg(Con, lgok([{nick, Nick}, {id, Id}, {stream, Stream}])),
			proc_lib:spawn(fun() -> send_broadcast(State#rstate.users, addr([{nick, Nick}, {id, Id}])) end),
			{reply, {ok, UState}, State#rstate{ nicks = State#rstate.nicks ++ [{Nick, Id}] }}
	end;

handle_call({pub, Con, S}, _From, State) -> 
	Msg = [{id, S#ustate.id}, {nick, S#ustate.nick}, {stream, S#ustate.stream}],
	case lists:any(fun(I) -> I == Msg end, State#rstate.video) of
		true -> {reply, S, State};
		false -> 
			RUsers = [ X || X <- State#rstate.users, X =/= Con ], 
			proc_lib:spawn(fun() -> send_broadcast(RUsers, show(Msg)) end),
			{reply, S, State#rstate{ video = State#rstate.video ++ [Msg] }}
	end;

handle_call({stop, _Con, S}, _From, State) -> 
	Msg = [{id, S#ustate.id}],
	NVideo = lists:filter(fun([I, _, _]) -> [I] =/= Msg end, State#rstate.video),
	case State#rstate.video == NVideo of
		true -> 
			{reply, S, State};
		false -> 
			proc_lib:spawn(fun() -> send_broadcast(State#rstate.users, hide(Msg)) end),
			{reply, S, State#rstate{ video = NVideo }}
	end;

handle_call({chat, _Con, S, M}, _From, State) -> 
	{Hour, Min, _} = time(),
	Tm = list_to_binary(integer_to_list(Hour) ++ ":" ++ integer_to_list(Min)),
	Msg = [{tm, Tm}, {nick, S#ustate.nick}, {msg, M}],
	proc_lib:spawn(fun() -> send_broadcast(State#rstate.users, chat(Msg)) end),
	{reply, S, State#rstate{ chat = State#rstate.chat  ++ [Msg] }};

handle_call({rm, Con, undefine}, _From, State) -> 
	NUsers = [ X || X <- State#rstate.users, X =/= Con ], 
	{reply, undefine, State#rstate{ users = NUsers }};

handle_call({rm, Con, S}, _From, State) -> 
	NUsers = [ X || X <- State#rstate.users, X =/= Con ], 
	NNicks = [ X || X <- State#rstate.nicks, X =/= {S#ustate.nick, S#ustate.id}], 
	NVideo = lists:filter(fun([{id, I}, _, _]) -> I =/= S#ustate.id end, State#rstate.video),
	proc_lib:spawn(fun() -> send_broadcast(NUsers, delr([{id,  S#ustate.id}])) end),
	{reply, undefine, State#rstate{ users = NUsers, nicks = NNicks, video = NVideo }};

handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) ->  {noreply, State}.

terminate(_Reason, State) -> 
	lists:map(fun(I) -> I:close() end, State#rstate.users),
	ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% local funs
%

user_init(Con, State) ->
	lists:map(fun({Nick, Id}) -> send_msg(Con, addr([{nick, Nick}, {id, Id}])) end, State#rstate.nicks),
	lists:map(fun(I) -> send_msg(Con, chat(I)) end, State#rstate.chat),
	lists:map(fun(I) -> send_msg(Con, show(I)) end, State#rstate.video).	

lgok(M) -> [{a, lgok}, {server, ?RTMP_HOST}] ++ M. 
lger(M) -> [{a, lger}] ++ M.
addr(M) -> [{a, addr}] ++ M.
delr(M) -> [{a, delr}] ++ M.
chat(M) -> [{a, chat}] ++ M.
show(M) -> [{a, show}, {server, ?RTMP_HOST}] ++ M.
hide(M) -> [{a, hide}] ++ M.

send_msg(Con, M) -> 
	{ok, R} = json:encode({M}),
	Con:send(R).

send_broadcast(List, M) ->
	lists:map(fun(I) -> send_msg(I, M) end, List).

token() -> list_to_binary(random_str(?TOKEN_LEN)).
random_str(Length) ->
	Alpha = "1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM",
	Size = length(Alpha),
	[lists:nth(random:uniform(Size), Alpha) || _ <- lists:seq(1, Length)].
