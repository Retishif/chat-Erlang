-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
   #client_st {gui = GUIName, nick = Nick, state = ?DISCONNECTED }.
%% ---------------------------------------------------------------------------

%% Connect to server (for DCHAT)
loop(St, {connect, {Server, Machine}}) ->
	case St#client_st.state of
		?CONNECTED ->
				{{error, user_already_connected, " ALREADY CONNECTED!!"}, St};
		?INCHAT -> {{error, user_already_connected, " ALREADY CONNECTED!! "}, St};
		?DISCONNECTED -> 
			case catch(genserver:request({to_atom(Server), to_atom(Machine)}, {connect, St#client_st.nick}, ?TIMEOUT)) of
				{'EXIT', _} -> {{error, server_not_reached, " NOT REACHED" }, St};
				user_already_connected -> {{error, user_already_connected, " ALREADY CONNECTED TO THE SERVER!! "}, St};%% unreachable code
				_ -> {ok, St#client_st {state = ?CONNECTED, server = {Server, Machine}}}
			end
	end;

%% Connect to server 
loop(St, {connect, Server}) ->
	case St#client_st.state of
		?CONNECTED ->
				{{error, user_already_connected, " ALREADY CONNECTED!!"}, St};
		?INCHAT -> {{error, user_already_connected, " ALREADY CONNECTED!! "}, St};
		?DISCONNECTED -> 
			case catch(genserver:request(to_atom(Server), {connect, St#client_st.nick}, ?TIMEOUT)) of
				{'EXIT', _} -> {{error, server_not_reached, " NOT REACHED" }, St};
				user_already_connected -> {{error, user_already_connected, " ALREADY CONNECTED TO THE SERVER!! "}, St};%% unreachable code
				_ -> {ok, St#client_st {state = ?CONNECTED, server = Server}}
			end
	end;

%% Disconnect from server
loop(St, disconnect) ->
	case St#client_st.state of
		?CONNECTED ->
			case catch(genserver:request(to_atom(St#client_st.server), {disconnect, St#client_st.nick}, ?TIMEOUT)) of
				{'EXIT', _} -> 
						{{error, server_not_reached, " NOT REACHED (state is now DISCONNECTED) "}, St#client_st {state = ?DISCONNECTED}};
				user_not_connected -> {{error, user_not_connected, " NOT CONNECTED TO THE SERVER!! "}, St};	%% unreachable code			 
				_ -> {ok,  St#client_st {state = ?DISCONNECTED}}
			end;
		?INCHAT -> {{error, leave_channels_first, "NEED TO LEAVE FIRST!! "}, St}; 
		?DISCONNECTED -> {{error, user_not_connected, " NOT CONNECTED "}, St}		
	end;    

% Join channel
loop(St, {join, Channel}) ->
	 case St#client_st.state of
		?CONNECTED ->
			{_ChannelPid, ClientPid} = generate(St, Channel),
			case catch(genserver:request(to_atom(St#client_st.server), {join, Channel, ClientPid}, ?TIMEOUT)) of
				{'EXIT', _} -> {{error, server_not_reached, " NOT REACHED (state is now DISCONNECTED) "}, St#client_st {state = ?DISCONNECTED}};
				user_already_joined -> {error, user_already_joined, "ALREADY JOINED!! "}; %% unreachable code
				ok -> {ok,  St#client_st {state = ?INCHAT, count = St#client_st.count + 1}}
			end;
		?INCHAT -> 
			{_ChannelPid, ClientPid} = generate(St, Channel),
			case catch(genserver:request(to_atom(St#client_st.server), {join, Channel, ClientPid}, ?TIMEOUT)) of
				{'EXIT', _} -> {{error, server_not_reached, " NOT REACHED (state is now DISCONNECTED) "}, St#client_st {state = ?DISCONNECTED}};
				user_already_joined -> {{error, user_already_joined, "ALREADY JOINED!! "}, St};
				ok -> {ok,  St#client_st {count = St#client_st.count + 1}}
			end;
		?DISCONNECTED -> {{error, user_not_connected, "NEED TO CONNECT FIRST!! "}, St}		
	end;       

%% Leave channel. The state will change to CONNECTED when the client leaves all its channels
loop(St, {leave, Channel}) ->
	 case St#client_st.state of
		?CONNECTED -> {{error, user_not_joined, "NEED TO JOIN FIRST!! "}, St};
		?INCHAT -> 
			{_ChannelPid, ClientPid} = generate(St, Channel),
			case catch(genserver:request(to_atom(St#client_st.server), {leave, Channel, ClientPid}, ?TIMEOUT)) of
				{'EXIT', _} -> {{error, server_not_reached, " NOT REACHED (state is now DISCONNECTED) "}, St#client_st {state = ?DISCONNECTED}};
				user_not_joined -> {{error, user_not_joined, "NOT JOINED TO THIS CHANNEL!! "}, St};
				ok -> 
					case St#client_st.count of
						1 -> {ok,  St#client_st {state = ?CONNECTED, count = St#client_st.count - 1}}; 
						_ -> {ok,  St#client_st {count = St#client_st.count - 1}}
					end
			end;
		?DISCONNECTED -> {{error, user_not_connected, "NEED TO CONNECT FIRST!! "}, St}		
	end;       

% Sending messages. The client sends msgs directly to the channels inorder to make it more robust
loop(St, {msg_from_GUI, Channel, Msg}) ->
	case St#client_st.state of
		?CONNECTED -> {{error, user_not_joined, "NEED TO JOIN FIRST!! "}, St};
		?INCHAT -> 
			{ChannelPid, ClientPid} = generate(St, Channel),
			case catch(genserver:request(ChannelPid, {msg, {ClientPid, St#client_st.nick}, Msg}, ?TIMEOUT)) of
				{'EXIT', _} -> {{error, server_not_reached, " NOT REACHED (state is now DISCONNECTED) "}, St#client_st {state = ?DISCONNECTED}};
				ok -> {ok,  St};
				user_not_joined -> {{error, user_not_joined, "NEED TO JOIN FIRST!! "}, St} %% unreachable code
			end;
		?DISCONNECTED -> {{error, user_not_connected, "NEED TO CONNECT FIRST!! "}, St}		
	end;        

%% Get current nick name
loop(St, whoami) ->
    {St#client_st.nick, St};

%% Change nick name. Happens only in DISCONNECTED state
loop(St, {nick, Nick}) ->
	case St#client_st.state of
		?DISCONNECTED ->
				{ok,  St#client_st {nick = Nick}};
		_ -> {{error, user_already_connected, "NEED TO DISCONNECT FIRST!! "}, St}
	end;

%% Incoming message
loop(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.

%%makes the required configuration depending on config
to_atom(String) ->
	case String of
		{Server, Machine} -> {list_to_atom(Server), list_to_atom(Machine)};
		_ -> list_to_atom(String)
	end.

%%makes the required configuration depending on config
generate(St, Channel)->
	case St#client_st.server of
				{_, Machine} -> ChannelPid = {to_atom(Channel), to_atom(Machine)};
				_ -> ChannelPid = to_atom(Channel)
	end,
	{ChannelPid, self()}.
