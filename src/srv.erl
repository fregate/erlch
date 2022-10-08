-module(srv).

-include("common.hrl").

-export([start/1, stop/1, impl/2]).

start(Token) ->
	io:format("server private token for administration: ~p~n", [Token]),
	register(msg_server, spawn(srv, impl, [Token, []])),
	msg_server ! {start}.

stop(Token) ->
	msg_server ! {stop, Token},
	ok.

impl(Token, Clients) ->
	receive
		{start} ->
			io:format("starting server... ~p~n", [self()]),
			impl(Token, Clients);
		{connect, ClientPID, Name} ->
			UpdatedClients = connect(ClientPID, Name, Clients),
			io:format("new connection, list of clients:~n~p~n", [UpdatedClients]),
			impl(Token, UpdatedClients);
		{disconnect, ClientPID} ->
			io:format("disconnect client: ~p~n", [ClientPID]),
			UpdatedClients = disconnect(ClientPID, Clients),
			io:format("client disconnected, list of clients:~n~p~n", [UpdatedClients]),
			impl(Token, UpdatedClients);
		{list, ClientPID} ->
			io:format("client request clients list~n"),
			ClientPID ! {clients, Clients},
			impl(Token, Clients);
		{direct, FromPID, ToName, Message} ->
			io:format("receive direct message~n"),
			case lists:search(fun(C) ->
				C#client.pid == FromPID
			end,
			Clients) of
				{value, From} -> 
					case lists:search(fun(C) ->
						C#client.name == ToName
					end,
					Clients) of
						{value, To} -> send_message(From, To, Message);
						false -> send_error(FromPID, "Unknown recipient!")
					end;
				false ->
					send_error(FromPID, "Unknown sender!")
			end,
			impl(Token, Clients);
		{all, FromPID, Message} ->
			io:format("receive broadcast message~n"),
			{value, From} = lists:search(fun(C) ->
				C#client.pid == FromPID
			end,
			Clients),
			send_broadcast_message(From, Clients, Message),
			impl(Token, Clients);
		{stop, PrivateToken} when Token == PrivateToken ->
			io:format("stop server~n"),
			send_broadcast_message(#client{name = "> Server <", pid = self()}, Clients, "Server is shuttings down..."),
			ok;
		_ ->
			io:format("error: recieved unknown command"),
			impl(Token, Clients)
	after
		600000 ->
			io:format("timeout~n"),
			ok
	end.

connect(NewClientPID, Name, ClientList) ->
	io:format("new client: ~p~n", [NewClientPID]),
	lists:append([[#client{pid = NewClientPID, name = Name}], ClientList]).

disconnect(ClientPID, ClientList) ->
	lists:filter(fun(E) ->
			E#client.pid /= ClientPID
		end, ClientList).

send_message(From, To, Message) when is_record(From, client) and is_record(To, client) ->
	io:format("send to client direct message. [~p, ~p]~n", [From#client.name, To#client.name]),
	To#client.pid ! {message, {From#client.name, Message}}.

send_broadcast_message(_, [], _) ->
	ok;
send_broadcast_message(From, [To|Others], Message) when is_record(From, client) and is_record(To, client) and From#client.pid == To#client.pid ->
	send_broadcast_message(From, Others, Message);
send_broadcast_message(From, [To|Others], Message) ->
	send_message(From, To, Message),
	send_broadcast_message(From, Others, Message).

send_error(FromPID, ErrorMessage) ->
	FromPID ! {error, ErrorMessage}.