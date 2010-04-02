%%%-------------------------------------------------------------------
%%% Description : balancer for emongo_pool connections
%%%-------------------------------------------------------------------
-module(emongo_balancer).

-behaviour(gen_server).

%% API
-export([start_link/2, pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id,
                active,
                passive,
                timer=undefined
               }).

-define(POOL_ID(BalancerId, PoolIdx), {BalancerId, PoolIdx}).
-define(RECHECK_TIME, 10000).

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
    gen_server:call(BalancerPid, pid, infinity).

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
    process_flag(trap_exit, true),
    self() ! {init, Pools},
    {ok, #state{id = BalId,
                active = queue:new(),
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
handle_call(pid, From, #state{id=BalId, active=Active, passive=Passive, timer=Timer}=State) ->
    case queue:out(Active) of
        {{value, PoolIdx}, Q2} ->
            case emongo_sup:worker_pid(?POOL_ID(BalId, PoolIdx)) of
                undefined ->
                    handle_call(pid, From,
                                State#state{active=Q2,
                                            passive=[PoolIdx | Passive],
                                            timer=set_timer(Timer)
                                           });
                Pid ->
                    {reply, Pid, State#state{active=queue:in(PoolIdx, Q2)}}
            end;
        {empty, _} ->
            {reply, undefined, State}
    end;

handle_call(stop_children, _, #state{id=BalId, active=Active, passive=Passive}=State) ->
    Fun = fun(PoolIdx) ->
                  emongo_sup:stop_pool(?POOL_ID(BalId, PoolIdx)),
                  false
          end,
    lists:foreach(Fun, Passive),
    
    {reply, ok, State#state{active=queue:filter(Fun, Active), passive=[]}};

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
    Fun = fun({Host, Port, Database, Size}, {PoolIdx, PoolQueue}) ->
                  case emongo_sup:start_pool(?POOL_ID(BalId, PoolIdx),
                                             Host, Port, Database, Size) of
                      {ok, PoolPid} ->
                          link(PoolPid),
                          {PoolIdx + 1, queue:in(PoolIdx, PoolQueue)};
                      _ ->
                          {PoolIdx + 1, PoolQueue}
                  end
          end,
    {_, PoolQueue} = lists:foldl(Fun, {1, Active}, Pools),
    {noreply, State#state{active=PoolQueue}};

handle_info(recheck, State) ->
    {noreply, activate(State, [])};

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

set_timer(undefined) ->
    erlang:send_after(?RECHECK_TIME, self(), recheck);
set_timer(TimerRef) ->
    TimerRef.


activate(#state{passive=[], timer=_TimerRef}=State, []) ->
    State#state{timer=undefined};

activate(#state{passive=[]}=State, Passive) ->
    State#state{passive=Passive, timer=erlang:send_after(?RECHECK_TIME, self(), recheck)};

activate(#state{id=BalId, active=Active, passive=[PoolIdx | Passive]}=State, Acc) ->
    case emongo_sup:worker_pid(?POOL_ID(BalId, PoolIdx)) of
        undefined ->
            activate(State#state{passive=Passive}, [PoolIdx | Acc]);
        _ ->
            activate(State#state{active=queue:in(PoolIdx, Active), passive=Passive}, Acc)
    end.
