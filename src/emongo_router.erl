%%%-------------------------------------------------------------------
%%% Description : balancer for emongo_pool connections
%%%-------------------------------------------------------------------
-module(emongo_router).

-behaviour(gen_server).

%% API
-export([start_link/2, pid/1, pid/2]).

-deprecated([pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id,
                active,
                passive,
                timer=undefined
               }).

-define(POOL_ID(BalancerId, PoolIdx), {BalancerId, PoolIdx}).
-define(RECHECK_TIME, 9500).

%% messages
-define(pid(RequestCount), {pid, RequestCount}).

%% to be removed next release
-define(old_pid(), pid).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(BalId, Pools) ->
    gen_server:start_link(?MODULE, [BalId, Pools], []).


pid(BalancerPid) ->
    pid(BalancerPid, 1).


pid(BalancerPid, RequestCount) ->
    gen_server:call(BalancerPid, {pid, RequestCount}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([BalId, Pools]) ->
    self() ! {init, Pools},
    {ok, #state{id = BalId,
                active = [],
                passive = []}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messagesp
%%--------------------------------------------------------------------
handle_call(?old_pid(), _From, State) ->
    {Pid, NewState} = get_pid(State, emongo_sup:pools(), 1),
    {reply, Pid, NewState};

handle_call(?pid(RequestCount), _From, State) ->
    {Pid, NewState} = get_pid(State, emongo_sup:pools(), RequestCount),
    {reply, Pid, NewState};

handle_call(stop_children, _, #state{id=BalId, active=Active, passive=Passive}=State) ->
    Fun = fun(PoolIdx) ->
                  emongo_sup:stop_pool(?POOL_ID(BalId, PoolIdx)),
                  false
          end,
    lists:foreach(Fun, Passive),
    lists:foreach(Fun, Active),
    
    {reply, ok, State#state{active=[], passive=[]}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({init, Pools}, #state{id=BalId, active=Active}=State) ->
    Fun = fun({Host, Port, Database, Size}, {PoolIdx, PoolList}) ->
                  case emongo_sup:start_pool(?POOL_ID(BalId, PoolIdx),
                                             Host, Port, Database, Size) of
                      {ok, _} ->
                          ok;
                      {error, {already_started, _}} ->
                          ok
                  end,
                  {PoolIdx + 1, [PoolIdx | PoolList]}
          end,
    {_, PoolList} = lists:foldl(Fun, {1, Active}, Pools),
    {noreply, State#state{active=lists:sort(PoolList)}};

handle_info(recheck, State) ->
    {noreply, activate(State, [], emongo_sup:pools())};

handle_info(_Info, State) ->
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_pid(#state{id=BalId, active=Active, passive=Passive, timer=Timer}=State, Pools, RequestCount) ->
    case Active of
        [PoolIdx | Active2] ->
            case emongo_sup:worker_pid(?POOL_ID(BalId, PoolIdx), Pools, RequestCount) of
                undefined ->
                    error_logger:info_msg("pool ~p is disabled!~n", [?POOL_ID(BalId, PoolIdx)]),
                    
                    get_pid(State#state{active=Active2,
                                        passive=[PoolIdx | Passive],
                                        timer=set_timer(Timer)
                                       }, Pools, RequestCount);
                Pid ->
                    {Pid, State}
            end;
        [] ->
            {undefined, State}
    end.


set_timer(undefined) ->
    erlang:send_after(?RECHECK_TIME, self(), recheck);
set_timer(TimerRef) ->
    TimerRef.


activate(#state{passive=[], timer=_TimerRef}=State, [], _) ->
    State#state{timer=undefined};

activate(#state{passive=[]}=State, Passive, _) ->
    State#state{passive=Passive, timer=erlang:send_after(?RECHECK_TIME, self(), recheck)};

activate(#state{id=BalId, active=Active, passive=[PoolIdx | Passive]}=State, Acc, Pools) ->
    case emongo_sup:worker_pid(?POOL_ID(BalId, PoolIdx), Pools, 0) of
        undefined ->
            activate(State#state{passive=Passive}, [PoolIdx | Acc], Pools);
        _ ->
            error_logger:info_msg("pool ~p is enabled!~n", [?POOL_ID(BalId, PoolIdx)]),
            activate(State#state{active=lists:umerge([PoolIdx], Active), passive=Passive}, Acc, Pools)
    end.
