%%% -------------------------------------------------------------------
%%% Author  : mkrentovskiy
%%% Created : 24.08.2010
%%% -------------------------------------------------------------------
-module(vico_room).
-behaviour(gen_server).

-include("vico.hrl").

-export([a_init/1, a_enter/3, a_exit/1, a_chat/2, out/1]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

a_init(UserInfo) -> gen_server:call(vico_room, {init, UserInfo}).
a_enter(UserInfo, Type, CamUrl) -> gen_server:call(vico_room, {enter, UserInfo, Type, CamUrl}).
a_exit(UserInfo) -> gen_server:call(vico_room, {exit, UserInfo}).
a_chat(UserInfo, Text) -> gen_server:call(vico_room, {chat, UserInfo, Text}).
out(UserInfo) -> gen_server:call(vico_room, {out, UserInfo}).


%% ====================================================================
%% Server functions
%% ====================================================================

start() ->
	gen_server:start_link({local, vico_room}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #room{}}.

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
handle_call({init, UserInfo}, _From, State) ->  	          
  	Others = dict:erase(UserInfo#userinfo.pid, State#room.active), 	
  	ActiveUsers = dict:fold(fun(_, U, AU) ->   
  			Msgs = msgs_enter(U),
  			lists:merge(AU, Msgs)		
  		end, [], Others),
	
	AllUsers = dict:fold(fun(_, U, AU) ->   
  			Msgs = msgs_cd(connect, U),
  			lists:merge(AU, Msgs)		
  		end, [], State#room.users),
	InitMsgs = lists:merge(lists:merge(ActiveUsers, AllUsers), State#room.chat),

	CMsg = msgs_cd(connect, UserInfo),
	broadcast_messages(UserInfo#userinfo.pid, State#room.users, CMsg),
   	
	case dict:find(UserInfo#userinfo.pid, State#room.users) of 
   		{ok, _} ->
			{reply, InitMsgs, State};
   		error -> 
   			Users = dict:store(UserInfo#userinfo.pid, UserInfo, State#room.users),
   			{reply, InitMsgs, State#room{ users = Users }}
   	end;

handle_call({enter, UserInfo, _Type, _CamUrl}, _From, State) ->
	%%case Type of 
	%%	<<"">>  ->
    	Msgs = msgs_enter(UserInfo),		
		broadcast_messages(UserInfo#userinfo.pid, State#room.users, Msgs),
		NewActive = dict:store(UserInfo#userinfo.pid, UserInfo, State#room.active),    			
	%%end;	
    case dict:find(UserInfo#userinfo.pid, State#room.active) of 
   		{ok, _} ->
   			{reply, ok, State};
   		error -> 
			NewActive = dict:store(UserInfo#userinfo.pid, UserInfo, State#room.active),    			
   			{reply, ok, State#room{active = NewActive}}
   	end;

handle_call({exit, UserInfo}, _From, State) ->
    NewActive = user_exit(UserInfo, State),
    {reply, ok, State#room{active = NewActive}};

handle_call({chat, UserInfo, Text}, _From, State) ->
    ChatMsg = ?S([{action, chat}, {title, UserInfo#userinfo.title}, {text, Text}]),
    Pid = UserInfo#userinfo.pid,
	broadcast_messages(Pid, State#room.users, [ChatMsg]),
    ChatMsgs = State#room.chat ++ [ChatMsg],
    {reply, ok, State#room{chat = ChatMsgs}};
    
handle_call({out, UserInfo}, _From, State) ->
    NewActive = user_exit(UserInfo, State),

	CMsg = msgs_cd(disconnect, UserInfo),
	broadcast_messages(UserInfo#userinfo.pid, State#room.users, CMsg),

	NewUsers = dict:erase(UserInfo#userinfo.pid, State#room.users),
    {reply, ok, State#room{active = NewActive, users = NewUsers}}.          

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

msgs_enter(UserInfo) ->
	StreamId = unicode:characters_to_binary(UserInfo#userinfo.streamid),
	[?S([{action, enter}, 
    		{url, <<?RTMP_SERVER/binary, StreamId/binary>>},
			{server, ?RTMP_SERVER},
			{stream, StreamId},
    		{id, UserInfo#userinfo.login},
    		{title, UserInfo#userinfo.title},
    		{style, UserInfo#userinfo.group}])].

msgs_cd(Action, UserInfo) ->
	[?S([{action, Action}, 
    		{id, UserInfo#userinfo.login},
    		{title, UserInfo#userinfo.title}])].

user_exit(UserInfo, State) ->
	Pid = UserInfo#userinfo.pid,
	Msgs = [?S([{action, exit}, 
   			{id, UserInfo#userinfo.login}])],
	broadcast_messages(Pid, State#room.users, Msgs),
	dict:erase(Pid, State#room.active).    				

broadcast_messages(From, Users, Msgs) ->
	Others = dict:erase(From, Users),
 	dict:fold(fun(_, U, Msgs) ->
    		vico_user:add_messages(U#userinfo.pid, Msgs),
			Msgs
 		end, Msgs, Others).   

broadcast_messages_to_all(Users, Msgs) ->
 	dict:fold(fun(_, U, Msgs) ->
    		vico_user:add_messages(U#userinfo.pid, Msgs),
			Msgs
 		end, Msgs, Users).   
