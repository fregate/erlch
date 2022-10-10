-module(srv_app).
-behaviour(application).
-export([start/2, stop/1, impl/1]).

-include("common.hrl").

start(_StartType, _StartArgs) ->
	register(msg_server, spawn(srv_app, impl, [[]])),
	msg_server ! {start},
	srv_sup:start_link().

stop(_State) ->
	msg_server ! {stop},
	ok.

impl(Clients) ->
	receive
		{start} ->
			io:format("starting server... ~p~n", [self()]),
			impl(Clients);
		{connect, ClientPID, Name} ->
			UpdatedClients = connect(ClientPID, Name, Clients),
			io:format("new connection, list of clients:~n~p~n", [UpdatedClients]),
			impl(UpdatedClients);
		{disconnect, ClientPID} ->
			io:format("disconnect client: ~p~n", [ClientPID]),
			UpdatedClients = disconnect(ClientPID, Clients),
			io:format("client disconnected, list of clients:~n~p~n", [UpdatedClients]),
			impl(UpdatedClients);
		{list, ClientPID} ->
			io:format("client request clients list~n"),
			ClientPID ! {clients, Clients},
			impl(Clients);
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
			impl(Clients);
		{all, FromPID, Message} ->
			io:format("receive broadcast message~n"),
			{value, From} = lists:search(fun(C) ->
				C#client.pid == FromPID
			end,
			Clients),
			send_broadcast_message(From, Clients, Message),
			impl(Clients);
		{stop} ->
			io:format("stop server~n"),
			send_broadcast_message(#client{name = "> Server <", pid = self()}, Clients, "Server is shuttings down..."),
			ok;
		_ ->
			io:format("error: recieved unknown command"),
			impl(Clients)
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