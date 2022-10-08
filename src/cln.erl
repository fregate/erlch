-module(cln).

-include("common.hrl").

-export([
	connect/2,
	disconnect/0,
	list/0,
	message/2,
	all/1,
	impl/1
]).

connect(ServerPID, Name) ->
	register(client, spawn(cln, impl, [ServerPID])),
	client ! {connect, Name}.

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
		{connect, Name} ->
			{msg_server, ServerNode} ! {connect, self(), Name},
			impl(ServerNode);
		{stop} ->
			{msg_server, ServerNode} ! {disconnect, self()},
			ok;
		{list} ->
			{msg_server, ServerNode} ! {list, self()},
			impl(ServerNode);
		{clients, Clients} ->
			io:format("list of clients:~n~p~n", [Clients]),
			impl(ServerNode);
		{message, {Name, Message}} ->
			io:format("received message >~nfrom: ~p~n~p~n", [Name, Message]),
			impl(ServerNode);
		{direct, To, Message} ->
			{msg_server, ServerNode} ! {direct, self(), To, Message},
			impl(ServerNode);
		{broadcast, Message} ->
			{msg_server, ServerNode} ! {all, self(), Message},
			impl(ServerNode);
		{error, ErrorMessage} ->
			io:format("ERROR > ~p~n", [ErrorMessage]),
			impl(ServerNode);
		_ ->
			io:format("unknown api call~n")
	end.
