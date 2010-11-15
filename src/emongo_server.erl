-module(emongo_server).

-behaviour(gen_server).

-include("emongo.hrl").

-export([start_link/3]).

-export([send/3, send/2, send_recv/4]).
-export([send_recv_nowait/3, recv/4]).

-deprecated([send/3]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_id, socket, requests, leftover}).

%% messages
-define(abort(ReqId), {abort, ReqId}).
-define(send(Packet), {send, Packet}).
-define(send_recv(ReqId, Packet, From),
        {send_recv, ReqID, Packet, From}).

%% to be removed next release
-define(old_send(ReqId, Packet), {send, ReqId, Packet}).


start_link(PoolId, Host, Port) ->
    gen_server:start_link(?MODULE, [PoolId, Host, Port], []).


send(Pid, _ReqID, Packet) ->
    send(Pid, Packet).

send(Pid, Packet) ->
    gen_server:cast(Pid, ?send(Packet)).


send_recv_nowait(Pid, ReqID, Packet) ->
    Tag = make_ref(),
    gen_server:cast(Pid, ?send_recv(ReqID, Packet, {self(), Tag})),
    Tag.


recv(Pid, ReqID, 0, Tag) ->
    Pid ! ?abort(ReqID),
    receive
        {Tag, Resp} ->
            Documents = emongo_bson:decode(Resp#response.documents),
            Resp#response{documents=Documents}
    after 0 ->
            exit(emongo_timeout)
    end;

recv(Pid, ReqID, Timeout, Tag) ->
    receive
        {Tag, Resp} ->
            Documents = emongo_bson:decode(Resp#response.documents),
            Resp#response{documents=Documents}
    after Timeout ->
            recv(Pid, ReqID, 0, Tag)
    end.


send_recv(Pid, ReqID, Packet, Timeout) ->
    Tag = send_recv_nowait(Pid, ReqID, Packet),
    recv(Pid, ReqID, Timeout, Tag).


%% gen_server %%

init([PoolId, Host, Port]) ->
    case gen_tcp:connect(Host, Port, [binary, {active, true}, {nodelay, true}], ?TIMEOUT) of
        {ok, Socket} ->
            {ok, #state{pool_id=PoolId, socket=Socket, requests=[], leftover = <<>>}};
        {error, Reason} ->
            {stop, {failed_to_open_socket, Reason}}
    end.


handle_call(_Request, _From, State) ->
    {reply, undefined, State}.


handle_cast(?send_recv(ReqID, Packet, From), State) ->
    case is_aborted(ReqID) of
        true ->
            {noreply, State};
        _ ->
            gen_tcp:send(State#state.socket, Packet),
            State1 = State#state{requests=[{ReqID, From} | State#state.requests]},
            {noreply, State1}
    end;

handle_cast(?old_send(_ReqId, Packet), State) ->
    gen_tcp:send(State#state.socket, Packet),
    {noreply, State};

handle_cast(?send(Packet), State) ->
    gen_tcp:send(State#state.socket, Packet),
    {noreply, State}.


handle_info(?abort(ReqId), #state{requests=Requests}=State) ->
    State1 = State#state{requests=lists:keydelete(ReqId, 1, Requests)},
    {noreply, State1};

handle_info({tcp, _Socket, Data}, State) ->
    Leftover = <<(State#state.leftover)/binary, Data/binary>>,
    {noreply, process_bin(State#state{leftover= <<>>}, Leftover)};

handle_info({tcp_closed, _Socket}, _State) ->
    exit(tcp_closed);

handle_info({tcp_error, _Socket, Reason}, _State) ->
    exit({tcp_error, Reason}).


terminate(_, State) -> gen_tcp:close(State#state.socket).


code_change(_Old, State, _Extra) -> {ok, State}.

%% internal


process_bin(State, <<>>) ->
    State;

process_bin(State, Bin) ->
    case emongo_packet:decode_response(Bin) of
        undefined ->
            State#state{leftover=Bin};
        
        {Resp, Tail} ->
            ResponseTo = (Resp#response.header)#header.response_to,

            case lists:keytake(ResponseTo, 1, State#state.requests) of
                false ->
                    cleanup_cursor(Resp, ResponseTo, State),
                    process_bin(State, Tail);
                
                {value, {_, From}, Requests} ->
                    case is_aborted(ResponseTo) of
                        false ->
                            gen_server:reply(
                              From,
                              Resp#response{pool_id=State#state.pool_id});
                        true ->
                            cleanup_cursor(Resp, ResponseTo, State)
                    end,
                    process_bin(State#state{requests=Requests}, Tail)
            end
    end.


is_aborted(ReqId) ->
    receive
        ?abort(ReqId) ->
            true
    after 0 ->
            false
    end.

cleanup_cursor(#response{cursor_id=0}, _, _) ->
    ok;
cleanup_cursor(#response{cursor_id=CursorID}, ReqId, State) ->
    Packet = emongo_packet:kill_cursors(ReqId, [CursorID]),
    gen_tcp:send(State#state.socket, Packet).
