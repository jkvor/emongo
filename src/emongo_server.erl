-module(emongo_server).

-behaviour(gen_server).

-include("emongo.hrl").

-export([start_link/3, send/3, send_recv/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(request, {req_id, requestor}).
-record(state, {pool_id, socket, requests, leftover}).

start_link(PoolId, Host, Port) ->
	gen_server:start_link(?MODULE, [PoolId, Host, Port], []).

send(Pid, ReqID, Packet) ->
	case gen_server:cast(Pid, {send, ReqID, Packet}) of
		ok -> ok;
		{error, Reason} -> exit(Reason)
	end.

send_recv(Pid, ReqID, Packet, Timeout) ->
	case gen_server:call(Pid, {send_recv, ReqID, Packet}, Timeout) of
		{ok, Resp} ->
			Documents = emongo_bson:decode(Resp#response.documents),
			Resp#response{documents=Documents};
		{error, Reason} ->
			exit(Reason)
	end.

open_socket(Host, Port) ->
	case gen_tcp:connect(Host, Port, [binary, {active, true}, {nodelay, true}]) of
		{ok, Sock} ->
			Sock;
		{error, Reason} ->
			exit({failed_to_open_socket, Reason})
	end.

%% gen_server %%

init([PoolId, Host, Port]) ->
	Socket = open_socket(Host, Port),
	{ok, #state{pool_id=PoolId, socket=Socket, requests=[], leftover = <<>>}}.

handle_call({send_recv, ReqID, Packet}, From, State) ->
	gen_tcp:send(State#state.socket, Packet),
	Request = #request{req_id=ReqID, requestor=From},
	State1 = State#state{requests=[{ReqID, Request} | State#state.requests]},
	{noreply, State1}.

handle_cast({send, _, Packet}, State) ->
	gen_tcp:send(State#state.socket, Packet),
	{noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
	Leftover = <<(State#state.leftover)/binary, Data/binary>>,

	case emongo_packet:decode_response(Leftover) of
		undefined ->
			{noreply, State#state{leftover=Leftover}};
		{Resp, Tail} ->
			ResponseTo = (Resp#response.header)#header.response_to,

			case lists:keytake(ResponseTo, 1, State#state.requests) of
				false ->
					exit({unexpected_response, Resp});
				{value, {ResponseTo, Request}, Requests} ->
					gen_server:reply(Request#request.requestor, {ok, Resp}),
					{noreply, State#state{requests=Requests, leftover=Tail}}
			end
	end;
handle_info({tcp_closed, _Socket}, State) ->
	exit({State#state.pool_id, tcp_closed});
handle_info({tcp_error, _Socket, Reason}, State) ->
	exit({State#state.pool_id, Reason}).

terminate(_, State) -> gen_tcp:close(State#state.socket).

code_change(_Old, State, _Extra) -> {ok, State}.