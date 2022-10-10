-module(cln_app).
-behaviour(application).
-export([
	start/2,
	stop/1,
	connect/2,
	disconnect/0,
	list/0,
	message/2,
	all/1,
	impl/1
]).

start(_StartType, _StartArgs) ->
	register(client, spawn(cln_app, impl, [disconnected])),
	cln_sup:start_link().

stop(_State) ->
	disconnect(),
	ok.

connect(ServerPID, Name) ->
	client ! {connect, ServerPID, Name}.

disconnect() ->
	client ! {stop}.

list() ->
	client ! {list}.

message(To, Message) ->
	client ! {direct, To, Message}.

all(Message) ->
	client ! {broadcast, Message}.

impl(ServerNode) ->
	receive
		{connect, NewServerNode, Name} when ServerNode =:= disconnected ->
			{msg_server, NewServerNode} ! {connect, self(), Name},
			impl(NewServerNode);
		{stop} when ServerNode /= disconnected ->
			{msg_server, ServerNode} ! {disconnect, self()},
			ok;
		{list} when ServerNode /= disconnected ->
			{msg_server, ServerNode} ! {list, self()},
			impl(ServerNode);
		{clients, Clients} when ServerNode /= disconnected ->
			io:format("list of clients:~n~p~n", [Clients]),
			impl(ServerNode);
		{message, {Name, Message}} when ServerNode /= disconnected ->
			io:format("received message >~nfrom: ~p~n~p~n", [Name, Message]),
			impl(ServerNode);
		{direct, To, Message} when ServerNode /= disconnected ->
			{msg_server, ServerNode} ! {direct, self(), To, Message},
			impl(ServerNode);
		{broadcast, Message} when ServerNode /= disconnected ->
			{msg_server, ServerNode} ! {all, self(), Message},
			impl(ServerNode);
		{error, ErrorMessage} when ServerNode /= disconnected ->
			io:format("ERROR > ~p~n", [ErrorMessage]),
			impl(ServerNode);
		_ ->
			io:format("unknown api call. server node: ~p~n", [ServerNode])
	end.
