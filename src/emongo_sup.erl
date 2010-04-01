-module(emongo_sup).

-behaviour(supervisor).

-export([start_link/0, start_pool/5, stop_pool/1, pools/0, pool_pid/1]).

%% supervisor exports
-export([init/1]).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_pool(PoolId, Host, Port, Database, Size) ->
    supervisor:start_child(?MODULE, {PoolId,
		{emongo_pool, start_link, [PoolId, Host, Port, Database, Size]},
		permanent, 10000, worker, [emongo_pool]
	}).

stop_pool(PoolId) ->
    supervisor:terminate_child(?MODULE, PoolId),
    supervisor:delete_child(?MODULE, PoolId).

pools() ->
    [{PoolId, Pid} || {PoolId, Pid, _, [emongo_pool]} <- supervisor:which_children(?MODULE)].

pool_pid(PoolId) ->
    case [Pid || {Id, Pid, _, [emongo_pool]} <- supervisor:which_children(?MODULE), Id =:= PoolId] of
        [Pid] ->
            Pid;
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
