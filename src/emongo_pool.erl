%%%-------------------------------------------------------------------
%%% Description : emongo pool supervisor
%%%-------------------------------------------------------------------
-module(emongo_pool).

-behaviour(gen_server).

%% API
-export([start_link/5, pid/1, pid/2]).

-deprecated([pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("emongo.hrl").

-define(POLL_INTERVAL, 10000).
-define(POLL_TIMEOUT, 9000).

-record(pool, {id,
               host,
               port,
               database,
               size,
               active=true,
               poll=none,
               conn_pid=pqueue:new(),
               req_id=1}).

%% messages
-define(pid(RequestCount), {pid, RequestCount}).
-define(poll(), poll).
-define(poll_timeout(Pid, ReqId, Tag), {poll_timeout, Pid, ReqId, Tag}).

%% to be removed next release
-define(old_pid(), pid).


%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link(PoolId, Host, Port, Database, Size) ->
    gen_server:start_link(?MODULE, [PoolId, Host, Port, Database, Size], []).

pid(Pid) ->
    gen_server:call(Pid, pid).

pid(Pid, RequestCount) ->
    gen_server:call(Pid, {pid, RequestCount}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([PoolId, Host, Port, Database, Size]) ->
    process_flag(trap_exit, true),
    
    Pool0 = #pool{id = PoolId,
                 host = Host,
                 port = Port,
                 database = unicode:characters_to_binary(Database),
                 size = Size
                },
    
    {noreply, Pool} = handle_info(?poll(), Pool0),
    {ok, Pool}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(?old_pid(), _From, #pool{active=true}=State) ->
    {Reply, NewState} = get_pid(State, 1),
    {reply, Reply, NewState};

handle_call(?pid(RequestCount), _From, #pool{active=true}=State) ->
    {Reply, NewState} = get_pid(State, RequestCount),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #pool{conn_pid=Pids}=State) ->
    error_logger:error_msg("Pool ~p deactivated by worker death: ~p~n",
                           [State#pool.id, Reason]),
    
    Pids1 = pqueue:filter(fun(Item) -> Item =/= Pid end, Pids),
    {noreply, State#pool{conn_pid = Pids1, active=false}};

handle_info(?poll(), State) ->
    erlang:send_after(?POLL_INTERVAL, self(), poll),
    NewState = do_open_connections(State),
    {noreply, NewState};

handle_info(?poll_timeout(Pid, ReqId, Tag), #pool{poll={Tag, _}}=State) ->
    case catch emongo_server:recv(Pid, ReqId, 0, Tag) of
        #response{} ->
            {noreply, State#pool{active=true, poll=none}};
        _ ->
            {noreply, State#pool{active=false, poll=none}}
    end;

handle_info({Tag, _}, #pool{poll={Tag, TimerRef}}=State) ->
    _Time = erlang:cancel_timer(TimerRef),
    %%io:format("polling ~p success: ~p~n", [State#pool.id, Time]),
    {noreply, State#pool{active=true, poll=none}};

handle_info(Info, State) ->
    error_logger:info_msg("Pool ~p unknown message: ~p~n",
                           [State#pool.id, Info]),
    
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, State, _Extra) ->
    error_logger:info_msg("emongo_pool:code_change(~p, ...)~n", [OldVsn]),
    
    State1 = case queue:is_queue(State#pool.conn_pid) of
                 false ->
                     State;
                 true ->
                     State#pool{conn_pid = queue2pqueue(State#pool.conn_pid, pqueue:new())}
             end,
    
    {ok, State1}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
queue2pqueue(Queue, PQueue) ->
    case queue:out(Queue) of
        {empty, _} ->
            PQueue;
        {{value, Item}, NewQueue} ->
            queue2pqueue(NewQueue, pqueue:push(1, Item, PQueue))
    end.

get_pid(#pool{database=Database, conn_pid=Pids, req_id=ReqId}=State, RequestCount) ->
    case pqueue:pop(Pids) of
        {Pid, Q2} ->
            NewState = State#pool{conn_pid=pqueue:push(RequestCount, Pid, Q2),
                                  req_id=(ReqId + RequestCount)},
            {{Pid, Database, ReqId}, NewState};
        empty ->
            {undefined, State}
    end.

do_open_connections(#pool{conn_pid=Pids, size=Size}=Pool) -> 
    case pqueue:size(Pids) < Size of
        true ->
            case emongo_server:start_link(Pool#pool.id, Pool#pool.host, Pool#pool.port) of
                {error, _Reason} ->
                    Pool#pool{active=false};
                {ok, Pid} ->
                    do_open_connections(Pool#pool{conn_pid = pqueue:push(1, Pid, Pids)})
            end;
        false ->
            do_poll(Pool)
    end.

do_poll(Pool) ->
    case get_pid(Pool, 2) of
        {{Pid, Database, ReqId}, NewPool} ->
            PacketLast = emongo_packet:get_last_error(Database, ReqId),
            Tag = emongo_server:send_recv_nowait(Pid, ReqId, PacketLast),
            TimerRef = erlang:send_after(?POLL_TIMEOUT, self(), ?poll_timeout(Pid, ReqId, Tag)),
            NewPool#pool{poll={Tag, TimerRef}};
        _ ->
            Pool#pool{active=false}
    end.
