%%% -------------------------------------------------------------------
%%% Author  : mkrentovskiy
%%% Created : 23.08.2010
%%% -------------------------------------------------------------------
-module(vico_login_server).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("vico.hrl").

-export([check_session/1, check_user/2, init_db/0, users_list/0, add_user/4, delete_user/1]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

check_session(SessionId) ->
    gen_server:call(vico_login_server, {check_session, SessionId}).

check_user(Login, Password) ->
	gen_server:call(vico_login_server, {check_user, Login, Password}).

init_db() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(user, 
		[{disc_copies, [node()]}, 
		{attributes, record_info(fields, user)}]).

users_list() -> 
	gen_server:call(vico_login_server, {users_list}).

add_user(Login, Password, Group, Title) -> 
	gen_server:call(vico_login_server, {add_user, Login, Password, Group, Title}).

delete_user(Login) ->
	gen_server:call(vico_login_server, {delete_user, Login}).


%% ====================================================================
%% Server functions
%% ====================================================================

start() ->
	gen_server:start_link({local, vico_login_server}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, dict:new()}.

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

handle_call({check_user, Login, Password}, _From, State) ->	
    case find_user(Login, Password) of
		{user, UserInfo} ->		    
		    CheckedState = check_already_login(State, UserInfo),
		    SessionId = gen_session_id(),
			StreamId = gen_session_id(),
			Pid = vico_user:start(),
			AllUserInfo = UserInfo#userinfo{sid = SessionId, pid = Pid, streamid = StreamId}, 
			vico_user:add_user_info(Pid, AllUserInfo),			
		    NewState = dict:store(SessionId, Pid, CheckedState),			
		    {reply, {ok, AllUserInfo}, NewState};
		_ ->
	    	{reply, {error, account_does_not_exists}, State}
    end;

handle_call({check_session, SessionId}, _From, State) ->
    case dict:find(SessionId, State) of
	{ok, Value} ->
	    {reply, Value, State};
	error ->
	    {reply, session_does_not_exists, State}
    end;

handle_call({users_list}, _From, State) ->
    List = db_users_list(),
    {reply, List, State};

handle_call({add_user, Login, Password, Group, Title}, _From, State) ->
    db_add_user(Login, Password, Group, Title),
    List = db_users_list(),
    {reply, List, State};

handle_call({delete_user, Login}, _From, State) ->
    NewState = check_already_login(State, #userinfo{login = Login}),
    db_delete_user(Login),
    List = db_users_list(),
    {reply, List, NewState}.



%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	dict:map(fun(Key, Value) -> vico_user:out(Value) end, State),
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

find_user(Login, Password) ->
	PasswordMD5 = md5(Password),
	case mnesia:transaction(fun() -> mnesia:read({user, Login}) end) of
		{atomic, [{user, Login, PasswordMD5, Group, Title}]} -> 
			{user, #userinfo{login=Login, group=Group, title=Title}};					
		_ -> 
			{error}
	end.
	
check_already_login(Sessions, UserInfo)	->
	{NewSessions, _} = dict:fold(fun(SessionId, Pid, {Sessions, UserInfo}) ->
		case is_process_alive(Pid) of
			true -> 
				case vico_user:test_login(Pid, UserInfo#userinfo.login) of
					ok ->
						NewSessions = kill_session(Sessions, SessionId),
						{NewSessions, UserInfo};
					_ -> 
						{Sessions, UserInfo}
				end;
			false ->				 
				CleanSessions = kill_session(Sessions, SessionId),
				{CleanSessions, UserInfo}				
		end	 
		end, {Sessions, UserInfo}, Sessions),	
	NewSessions.

kill_session(Sessions, SessionId) ->
	case dict:find(SessionId, Sessions) of 	
		{ok, Pid} -> 
			case is_process_alive(Pid) of 
				true -> 
					vico_user:out(Pid),
					dict:erase(SessionId, Sessions);
				false ->
					dict:erase(SessionId, Sessions)
			end;
		error -> Sessions
	end.
		

db_users_list() -> 
	case mnesia:transaction(fun() -> qlc:e(qlc:q([X || X <- mnesia:table(user)])) end) of
		{atomic, List} -> List;
		_ -> []
	end.
	
db_add_user(Login, Password, Group, Title)->	
	PasswordMD5 = md5(Password),
	F = fun() -> mnesia:write({user, Login, PasswordMD5, Group, Title}) end,
	mnesia:transaction(F).
	
db_delete_user(Login) ->
	mnesia:transaction(fun() -> mnesia:delete({user, Login}) end).


gen_session_id() -> random_str(8, list_to_tuple("1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM")).

random_str(0, _Chars) -> [];
random_str(Len, Chars) -> [random_char(Chars)|random_str(Len-1, Chars)].
random_char(Chars) -> element(crypto:rand_uniform(1, tuple_size(Chars) + 1), Chars).

md5(S) ->
       Md5_bin =  erlang:md5(S),
       Md5_list = binary_to_list(Md5_bin),
       lists:flatten(list_to_hex(Md5_list)).
 
list_to_hex(L) ->
       lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 ->
       [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).

