-module(server).
-export([loop/2, initial_state/1, create_Channel/2, channel_loop/2]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(_) ->
    #server_st{}.

%% ---------------------SERVER PROCESS------------------------------------------------------

%registeres the client with its nick name in St
loop(St, {connect, Nick}) ->
	case [X || X <- St#server_st.clients, X =:= Nick] of
		[] -> {ok, St#server_st {clients = [Nick | St#server_st.clients]}};
		_ -> {user_already_connected, St}
	end;

%Unregisters the client from St
loop(St, {disconnect, Nick}) ->
	case [X || X <- St#server_st.clients, X =:= Nick] of
		[] -> {user_not_connected, St};
		_ -> {ok, St#server_st {clients = [X || X <- St#server_st.clients, X =/= Nick]}}
	end;

%communicates with the channel process to join the client to the channel. Channel will be created if not existed.
loop(St, {join, Channel, Pid}) ->
	case whereis(to_atom(Channel)) of
		undefined ->  genserver:start(to_atom(Channel), server:create_Channel(Channel, Pid), fun server:channel_loop/2),
			    	{ok, St};
		_ -> 
			case catch(genserver:request(to_atom(Channel), {join, Pid}, ?TIMEOUT)) of
				user_already_joined -> {user_already_joined, St};
				ok -> {ok,  St}
			end
	end;

%communicates with the channel process to remove the client from the channel.
loop(St, {leave, Channel, Pid}) ->
	case catch(genserver:request(to_atom(Channel), {leave, Pid}, ?TIMEOUT)) of
		user_not_joined -> {user_not_joined, St};
		ok -> {ok,  St}
	end.
%-------------------------------------------------------------------------------------------

%% ---------------------CHANNEL PROCESS------------------------------------------------------


create_Channel(Channel, Pid) ->
	#channel_st{channel = Channel, members = [Pid]}.

%allows the client to be a member (if not already been joined)
channel_loop(St, {join, Pid}) ->
	case [X || X <- St#channel_st.members, X =:= Pid] of
		[] -> {ok, St#channel_st {members = [Pid | St#channel_st.members]}};
		_ -> {user_already_joined, St}
	end;

%removes the client from membership (if already has been joined)
channel_loop(St, {leave, Pid}) ->
	case [X || X <- St#channel_st.members, X =:= Pid] of
		[] -> {user_not_joined, St};
		_ -> {ok, St#channel_st {members = [X || X <- St#channel_st.members, X =/= Pid]}}
	end;

%if client already has been joined, it spawns a seprate process to handel the broadcasting
channel_loop(St, {msg, {Pid, Nick}, Msg}) ->
	case [X || X <- St#channel_st.members, X =:= Pid] of
		[] -> {user_not_joined, St};
		_ -> spawn(fun() -> broadcast(St, Pid, Nick, Msg) end),
			{ok, St}
	end.
%-------------------------------------------------------------------------------------------

%% ---------------------BROADCASTING PROCESS------------------------------------------------------

%Execpt the sender, it sends the msg to all members of the channel concurrentlly
broadcast(St, Pid, Nick, Msg) ->
	[spawn(fun() -> genserver:request(X, {incoming_msg, St#channel_st.channel, Nick, Msg}, ?TIMEOUT) end) || 
								X <- St#channel_st.members, X =/= Pid].

%-------------------------------------------------------------------------------------------

to_atom(String) ->
    list_to_atom(String).

