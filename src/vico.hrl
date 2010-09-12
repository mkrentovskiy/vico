-define(RTMP_SERVER, <<"rtmp://127.0.0.1/rtmp/">>).
-define(MAX_MSGS, 120).
-define(CONTROL_KEY, <<"qwerty">>).
-define(SESSION_TIMEOUT, timer:seconds(60)).

-record(user, {login, password, group, title}).
-record(userinfo, {login, group, title, sid = "", pid = undefine, streamid = undefine}).
-record(state, {ui = {}, mq = [], timer = undefine}).
-record(room, {users = dict:new(), active = dict:new(), chat = []}).
-record(reply, {headers = [], body = [], state = {}}).

-define(REPLY_ADD_BODY(B),
	#reply{ headers = Reply#reply.headers,  body = Reply#reply.body ++ [B] }).
-define(REPLY_ADD_BODY_STATE(B), Reply#reply{ body = lists:merge(Reply#reply.body, B) }).
-define(REPLY_ADD_HEADER_BODY(H, B), 
	#reply{ headers = Reply#reply.headers ++ [H],  body = Reply#reply.body ++ [B] }).
-define(S(KV), {struct, KV}).

