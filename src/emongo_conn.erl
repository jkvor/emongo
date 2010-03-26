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
			#response{documents=[]};
        exit:ExitReason ->
            exit(ExitReason)
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
			#response{documents=[]};
        exit:ExitReason ->
            exit(ExitReason)
	end.
	
loop(State, Leftover) ->
	Socket = State#state.socket,
	receive
		{'$emongo_conn_send', {From, Mref}, {_ReqID, Packet}} ->
			gen_tcp:send(Socket, Packet),
			gen:reply({From, Mref}, ok),
			loop(State, Leftover);
		{'$emongo_conn_send_sync', {From, Mref}, {ReqID, Packet1, Packet2}} ->
			gen_tcp:send(Socket, Packet1),
			gen_tcp:send(Socket, Packet2), % This is the packet containing getlasterror
			Request = #request{req_id=ReqID, requestor={From, Mref}},
			State1 = State#state{requests=[{ReqID, Request}|State#state.requests]},
			loop(State1, Leftover);
		{'$emongo_conn_send_recv', {From, Mref}, {ReqID, Packet}} -> 
			gen_tcp:send(Socket, Packet),
			Request = #request{req_id=ReqID, requestor={From, Mref}},
			State1 = State#state{requests=[{ReqID, Request}|State#state.requests]},
			loop(State1, Leftover);
		{'$emongo_recv_timeout', {From, Mref}, ReqID} -> 
            case find_request(ReqID, State#state.requests, []) of
                {undefined, Others} ->
                    gen:reply({From, Mref}, ok),
                    loop(State#state{requests=Others}, Leftover);
                {_, Others} ->
                    gen:reply({From, Mref}, ok),
                    %Loop again, but drop any leftovers to 
                    %prevent the loop response processing
                    %from getting out of sync and causing all
                    %subsequent calls to send_recv to fail.
                    loop(State#state{requests=Others}, <<>>)
            end;
		{tcp, Socket, Data} ->
			case emongo_packet:decode_response(<<Leftover/binary, Data/binary>>) of
				undefined ->
					loop(State, <<Leftover/binary, Data/binary>>);
				{Resp, Tail} ->
					ResponseTo = (Resp#response.header)#header.response_to,
					case find_request(ResponseTo, State#state.requests, []) of
						{undefined, Others} ->
							%exit({unexpected_response, Resp});
							loop(State#state{requests=Others}, Tail);
						{Request, Others} ->
							gen:reply(Request#request.requestor, Resp),
							loop(State#state{requests=Others}, Tail)
					end
			end;
		{tcp_closed, Socket} ->
			exit({State#state.pool_id, tcp_closed});
		{tcp_error, Socket, Reason} ->
			exit({State#state.pool_id, Reason})
	end.
	
open_socket(Host, Port) ->
	case gen_tcp:connect(Host, Port, [binary, {active, true}]) of
		{ok, Sock} ->
			Sock;
		{error, Reason} ->
			exit({failed_to_open_socket, Reason})
	end.
	
find_request(RequestID, [{RequestID, Request}|Tail], OtherReqs) ->
	{Request, lists:append(OtherReqs, Tail)};

find_request(RequestID, [Request|Tail], OtherReqs) ->
	find_request(RequestID, Tail, [Request|OtherReqs]);
	
find_request(_RequestID, [], OtherReqs) ->
	{undefined, OtherReqs}.

