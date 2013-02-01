-module(room).
-behaviour(gen_server).

-export([msg/3, close/2, add_connection/3, rm_connection/2]).
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {uid = 0, c = []}).

%
% SockJS events
%

msg(<<"LOGIN:", Nick/binary>>, Con, S) when S == undefine -> 
	{ok, undefine};
msg(<<"">>, Con, S) when is_pid(S)  -> 
	{ok, S};
msg(_Msg, Con, S) -> 
	Con:send(<<"ERROR">>),
	Con:close(), 
	{ok, S}.

close(Con, Pid) when is_pid(Pid) ->
	rm_connection(Pid, Con),
	{ok, undefine};
close(_, _) -> {ok, undefine}.

%
% genserver wrappes
%

add_connection(Pid, Con) -> gen_server:call(Pid, {add, Con}).
rm_connection(Pid, Con) -> gen_server:call(Pid, {rm, Con}).

%
% genserver
%

start(Uid) -> 
	gen_server:start_link({global, ?NAME(Uid)}, ?MODULE, [Uid], []).

init([Uid]) ->
	{ok, #state{uid = Uid}}.

handle_call({add, Con}, _From, State) -> 
	{reply, ok, State#state{c = State#state.c ++ [{Con}]}};

handle_call({rm, Con}, _From, State) -> 
	NewC = lists:foldl(fun({C}, A) ->
			case compare(C, Con) of 
				true -> A;
				false -> A ++ [{C}]
			end
		end, [], State#state.c);

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(done, State) when length(State#state.c) == 0 -> 
	{stop, normal, State};
handle_info(_Info, State) ->  {noreply, State}.

terminate(_Reason, State) -> 
	lists:map(fun({C, _Tl}) -> C:close() end, State#state.c),
	ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% local funs
%

sha(Msg, Key) ->
	<<Mac:160/integer>> = crypto:sha_mac(Key, Msg),
	lists:flatten(io_lib:format("~40.16.0b", [Mac])).
