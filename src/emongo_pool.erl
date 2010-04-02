%%%-------------------------------------------------------------------
%%% Description : emongo pool supervisor
%%%-------------------------------------------------------------------
-module(emongo_pool).

-behaviour(gen_server).

%% API
-export([start_link/5, pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("emongo.hrl").
-define(RECONNECT_TIME, 10000).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link(PoolId, Host, Port, Database, Size) ->
    gen_server:start_link(?MODULE, [PoolId, Host, Port, Database, Size], []).

pid(Pid) ->
    gen_server:call(Pid, pid, infinity).

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
    
    Pool = #pool{id = PoolId,
                 host = Host,
                 port = Port,
                 database = Database,
                 size = Size
                },
    NewPool = do_open_connections(Pool),
    {ok, NewPool}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(pid, _From, #pool{conn_pid=Pids, req_id=ReqId}=State) ->
    case queue:out(Pids) of
        {{value, Pid}, Q2} ->
            NewState = State#pool{conn_pid=queue:in(Pid, Q2), req_id=(ReqId + 1)},
            {reply, {Pid, State}, NewState};
        {empty, _} ->
            {reply, undefined, State}
    end;
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
    case case Reason of
             tcp_closed -> true;
             {tcp_error, _Reason} -> true;
             _ -> false
         end
        of
        true ->
            Pids1 = queue:filter(fun(Item) -> Item =/= Pid end, Pids),

            NewState = do_open_connections(State#pool{conn_pid = Pids1}),
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(reconnect, State) ->
    NewState = do_open_connections(State),
    {noreply, NewState};

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

do_open_connections(#pool{conn_pid=Pids, size=Size}=Pool) -> 
    case queue:len(Pids) < Size of
        true ->
            case emongo_server:start_link(Pool#pool.id, Pool#pool.host, Pool#pool.port) of
                {error, _Reason} ->
                    erlang:send_after(?RECONNECT_TIME, self(), reconnect),
                    Pool;
                {ok, Pid} ->
                    do_open_connections(Pool#pool{conn_pid = queue:in(Pid, Pids)})
            end;
        false ->
            Pool
    end.
