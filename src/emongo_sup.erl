-module(emongo_sup).

-behaviour(supervisor).

-export([start_link/0, pools/0, worker_pid/2, worker_pid/3]).
-export([start_pool/5, stop_pool/1]).
-export([start_router/2, stop_router/1]).

-deprecated([worker_pid/2]).

%% supervisor exports
-export([init/1]).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_router(BalId, Pools) ->
    supervisor:start_child(?MODULE,
                           {BalId,
                            {emongo_router, start_link, [BalId, Pools]},
                            permanent, 10000, worker, [emongo_router]
                           }).

stop_router(BalId) ->
    case [Pid || {PoolId, Pid, _, [emongo_router]} <- supervisor:which_children(?MODULE), PoolId =:= BalId] of
        [Pid] ->
            gen_server:call(Pid, stop_children),
            stop_pool(BalId)
    end.


start_pool(PoolId, Host, Port, Database, Size) ->
    supervisor:start_child(?MODULE, {PoolId,
		{emongo_pool, start_link, [PoolId, Host, Port, Database, Size]},
		permanent, 10000, worker, [emongo_pool]
	}).


stop_pool(PoolId) ->
    supervisor:terminate_child(?MODULE, PoolId),
    supervisor:delete_child(?MODULE, PoolId).


pools() ->
    [{Id, Pid, Module} || {Id, Pid, _, [Module]}
                              <- supervisor:which_children(?MODULE)].

worker_pid(PoolId, Pools) ->
    worker_pid(PoolId, Pools, 1).


worker_pid(PoolId, Pools, RequestCount) ->
    case lists:keyfind(PoolId, 1, Pools) of
        {_, Pid, Module} ->
            Module:pid(Pid, RequestCount);
        _ ->
            undefined
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
	{ok, {{one_for_one, 10, 10}, [
		{emongo, {emongo, start_link, []},
		 permanent, 5000, worker, [emongo]}
	]}}.
