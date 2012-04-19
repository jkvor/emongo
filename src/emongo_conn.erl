%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(emongo_conn).

-export([start_link/3, init/4, send/3, send_sync/5, send_recv/4]).

-record(request, {req_id, requestor}).
-record(state, {pool_id, socket, requests}).

-include("emongo.hrl").

start_link(PoolId, Host, Port) ->
	proc_lib:start_link(?MODULE, init, [PoolId, Host, Port, self()]).

init(PoolId, Host, Port, Parent) ->
	Socket = open_socket(Host, Port),
	proc_lib:init_ack(Parent, self()),
	loop(#state{pool_id=PoolId, socket=Socket, requests=[]}, <<>>).

send(Pid, ReqID, Packet) ->
	case gen:call(Pid, '$emongo_conn_send', {ReqID, Packet}) of
		{ok, Result} -> Result;
		{error, Reason} -> exit(Reason)
	end.

send_sync(Pid, ReqID, Packet1, Packet2, Timeout) ->
	try
		{ok, Resp} = gen:call(Pid, '$emongo_conn_send_sync', {ReqID, Packet1, Packet2}, Timeout),
		Documents = emongo_bson:decode(Resp#response.documents),
		Resp#response{documents=Documents}
	catch
		exit:timeout->
			%Clear the state from the timed out call
			gen:call(Pid, '$emongo_recv_timeout', ReqID, Timeout),
			exit(timeout)
	end.

send_recv(Pid, ReqID, Packet, Timeout) ->
	try
		{ok, Resp} = gen:call(Pid, '$emongo_conn_send_recv', {ReqID, Packet}, Timeout),
		Documents = emongo_bson:decode(Resp#response.documents),
		Resp#response{documents=Documents}
	catch
		exit:timeout->
			%Clear the state from the timed out call
			gen:call(Pid, '$emongo_recv_timeout', ReqID, Timeout),
			exit(timeout)
	end.

loop(State, Leftover) ->
	{NewState, NewLeftover} = try
		Socket = State#state.socket,
		receive
			{'$emongo_conn_send', {From, Mref}, {_ReqID, Packet}} ->
				gen_tcp:send(Socket, Packet),
				gen:reply({From, Mref}, ok),
				{State, Leftover};
			{'$emongo_conn_send_sync', {From, Mref}, {ReqID, Packet1, Packet2}} ->
				% Packet2 is the packet containing getlasterror.
				% Send both packets in the same TCP packet for performance reasons.  It's
				% about 3 times faster.
				gen_tcp:send(Socket, <<Packet1/binary, Packet2/binary>>),
				Request = #request{req_id=ReqID, requestor={From, Mref}},
				State1 = State#state{requests=[{ReqID, Request}|State#state.requests]},
				{State1, Leftover};
			{'$emongo_conn_send_recv', {From, Mref}, {ReqID, Packet}} ->
				gen_tcp:send(Socket, Packet),
				Request = #request{req_id=ReqID, requestor={From, Mref}},
				State1 = State#state{requests=[{ReqID, Request}|State#state.requests]},
				{State1, Leftover};
			{'$emongo_recv_timeout', {From, Mref}, ReqID} ->
				case lists:keytake(ReqID, 1, State#state.requests) of
					false ->
						gen:reply({From, Mref}, ok),
						{State, Leftover};
					{value, _, Others} ->
						gen:reply({From, Mref}, ok),
						%Loop again, but drop any leftovers to
						%prevent the loop response processing
						%from getting out of sync and causing all
						%subsequent calls to send_recv to fail.
						%loop(State#state{requests=Others}, <<>>)

						% Leave Leftover there because it could be from a different
						% request than the one timing out.  This Pid is still in the
						% pool and can still be used by other processes.  If the
						% data gets out of sync, the socket needs to be closed and
						% reopened.
						{State#state{requests=Others}, Leftover}
				end;
			{tcp, Socket, Data} ->
				{_NewState, _NewLeftover} =
					process_bin(State, <<Leftover/binary, Data/binary>>);
			{tcp_closed, Socket} ->
				exit(tcp_closed);
			{tcp_error, Socket, Reason} ->
				exit(Reason)
		end
	catch _:Error ->
		% The exit message has to include the pool_id and follow a format the emongo
		% module expects so this process can be restarted.
		exit({?MODULE, State#state.pool_id, Error})
	end,
	loop(NewState, NewLeftover).

open_socket(Host, Port) ->
	case gen_tcp:connect(Host, Port, [binary, {active, true}]) of
		{ok, Sock} ->
			Sock;
		{error, Reason} ->
			exit({failed_to_open_socket, Reason})
	end.

process_bin(State, <<>>) ->
	{State, <<>>};
process_bin(State, Bin) ->
	case emongo_packet:decode_response(Bin) of
		undefined ->
			{State, Bin};
		{Resp, Tail} ->
			ResponseTo = (Resp#response.header)#header.response_to,
			NewState = case lists:keytake(ResponseTo, 1, State#state.requests) of
				false ->
					State;
				{value, {_ReqID, Request}, Others} ->
					gen:reply(Request#request.requestor, Resp),
					State#state{requests=Others}
			end,
			% Continue processing Tail in case there's another complete message
			% in it.
			process_bin(NewState, Tail)
	end.
