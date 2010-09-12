%%% -------------------------------------------------------------------
%%% Author  : mkrentovskiy
%%% Created : 25.08.2010
%%% -------------------------------------------------------------------
-module(vico_user).
-behaviour(gen_server).

-include("vico.hrl").

-export([process_action/1, test_login/2, add_messages/2, add_user_info/2, out/1]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

test_login(Pid, Login) ->
	gen_server:call(Pid, {test_login, Login}). 	 
add_user_info(Pid, UserInfo) -> gen_server:call(Pid, {add_user_info, UserInfo}).
get_user_info(Pid) -> gen_server:call(Pid, {get_user_info}).

process_action(Req) -> 
	case Req:get_cookie_value("SID") of
		undefined ->
			try_process(Req);
		Cookie ->
			case vico_login_server:check_session(Cookie) of
				session_does_not_exists ->
					try_process(Req);
				Pid ->
					process_action_accept(Pid, Req)
			end
	end.						
process_action_accept(Pid, Req) ->
	case is_process_alive(Pid) of
		true -> 
			gen_server:call(Pid, {process_action, Req});
		false ->				 
			Req:respond({501, [], []})
	end. 	 
add_messages(Pid, Msgs) -> 
	gen_server:call(Pid, {add_messages, Msgs}).

out(Pid) -> gen_server:cast(Pid, out).


%% ====================================================================
%% Server functions
%% ====================================================================

start() ->
	{ok, Pid} = gen_server:start(?MODULE, [], []),
	Pid.

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	{ok, Timer} = timer:send_after(?SESSION_TIMEOUT, out),
    {ok, #state{timer = Timer}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({add_user_info, UserInfo}, _From, State) ->
	{reply, ok, State#state{ui = UserInfo}};

handle_call({get_user_info}, _From, State) ->
    {reply, State#state.ui, State};

handle_call({test_login, Login}, _From, State) -> 
	UserInfo = State#state.ui,
	if
    	UserInfo#userinfo.login == Login -> {reply, ok, State};
    	UserInfo#userinfo.login =/= Login -> {reply, error, State}
    end;

handle_call({add_messages, Msgs}, _From, State) ->
	case length(State#state.mq) > ?MAX_MSGS of 
		true ->			
			{stop, mq_is_full, State};		
		false -> 
			NewMsgs = lists:merge(State#state.mq, Msgs),        
    		{reply, ok, State#state{mq = NewMsgs}}
	end;
    
handle_call({process_action, Req}, _From, State) ->        
	timer:cancel(State#state.timer),
	ProcessFun = fun(Msg, Reply) ->  
		State = Reply#reply.state,
		case Msg of
			?S([{<<"action">>, <<"init">>}]) -> ?REPLY_ADD_BODY_STATE(a_init(State));			
			?S([{<<"action">>, <<"start">>}, 
				{<<"type">>, Type},
				{<<"url">>, CamURL}]) -> 
					a_start(State, Type, CamURL), ?REPLY_ADD_BODY_STATE(pass_messages(State#state.mq));
			?S([{<<"action">>, <<"stop">>}]) -> a_stop(State), ?REPLY_ADD_BODY_STATE(pass_messages(State#state.mq));			
			?S([{<<"action">>, <<"chat">>}, 
				{<<"text">>, Text}]) -> 
					a_chat(State, Text), ?REPLY_ADD_BODY_STATE(pass_messages(State#state.mq));						
			?S([{<<"action">>, <<"ping">>}]) -> ?REPLY_ADD_BODY_STATE(pass_messages(State#state.mq));			
			_ -> ?REPLY_ADD_BODY_STATE([?S([{action, error}])])
		end					 	
	end,
	message_process(Req, ProcessFun, State),
	{ok, Timer} = timer:send_after(?SESSION_TIMEOUT, out),    
	{reply, ok, State#state{mq = [], timer = Timer}}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(out, State) -> {stop, normal, State};    
handle_cast(_Msg, State) -> {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(out, State) -> {stop, normal, State};
handle_info(_Info, State) -> {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	timer:cancel(State#state.timer),
    vico_room:out(State#state.ui),
    io:format("I, terminate! ~p~n", [Reason]),
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

try_process(Req) ->
	AuthFun = fun(Msg, Reply) ->  
			case Msg of
				?S([{<<"action">>, <<"auth">>}, 
						{<<"login">>, Login}, 
						{<<"password">>, Password}]) ->
					case vico_login_server:check_user(Login, Password) of
						{error, _Reason} -> 
							?REPLY_ADD_BODY(?S([{action, autherror}]));
						{ok, UserInfo} -> 
							?REPLY_ADD_HEADER_BODY(
								mochiweb_cookies:cookie("SID", UserInfo#userinfo.sid, [{path, "/"}]), 
								?S([{action, authok}]))
					end;
				%% control functions
				?S([{<<"action">>, <<"userslist">>}]) -> 
					UsersList = convert_users_list(vico_login_server:users_list()),
					?REPLY_ADD_BODY(?S([{action, userslist}, {users, UsersList}]));
				
				?S([{<<"action">>, <<"adduser">>},
					{<<"key">>, CONTROL_KEY},
					{<<"login">>, Login},
					{<<"password">>, Password},
					{<<"title">>, Title},
				    {<<"group">>, Group}]) -> 
					UsersList = convert_users_list(vico_login_server:add_user(Login, Password, Group, Title)),
					?REPLY_ADD_BODY(?S([{action, userslist}, {users, UsersList}]));
				
				?S([{<<"action">>, <<"deleteuser">>},
					{<<"key">>, CONTROL_KEY},
					{<<"login">>, Login}]) -> 
					UsersList = convert_users_list(vico_login_server:delete_user(Login)),
					?REPLY_ADD_BODY(?S([{action, userslist}, {users, UsersList}]));
				%% 
				
				_ -> ?REPLY_ADD_BODY(?S([{action, noauth}]))
			end			 	
		end,
	message_process(Req, AuthFun, {}).
	
convert_users_list(List) -> lists:foldl(fun(I, A) ->
		A ++ [?S([{<<"login">>, I#user.login}, {<<"group">>, I#user.group}, {<<"title">>, I#user.title}])]
	end, [], List).
	

message_process(Req, ProcessFun, State) ->
	try 
		Msgs = mochijson2:decode(proplists:get_value("m", Req:parse_post())),
		%% io:format("Incoming (message_process): ~p~n", [Msgs]),
		Reply = lists:foldl(ProcessFun, #reply{state=State}, Msgs),
		%% io:format("Outgoing (message_process): ~p~n~n", [Reply]),		
		Req:ok({"application/json", Reply#reply.headers, mochijson2:encode(Reply#reply.body)}),
		Req:cleanup()	
	catch
		_ -> Req:respond({502, [], []})
	end.

a_init(State) -> 
	UserInfo = State#state.ui,
	StreamId = unicode:characters_to_binary(UserInfo#userinfo.streamid),
	AuthMsg = [?S([{action, init}, 
		{server, ?RTMP_SERVER}, 
		{stream, StreamId},
		{title, UserInfo#userinfo.title},
		{login, UserInfo#userinfo.login}])],   
	RoomMsgs = lists:reverse(vico_room:a_init(State#state.ui)),
	lists:reverse(lists:merge(AuthMsg, RoomMsgs)).

a_start(State, Type, CamURL) -> vico_room:a_enter(State#state.ui, Type, CamURL).
a_stop(State) -> vico_room:a_exit(State#state.ui).		
a_chat(State, Text) -> vico_room:a_chat(State#state.ui, Text).

pass_messages([]) -> [?S([{action, pong}])];
pass_messages(L) -> L.

		