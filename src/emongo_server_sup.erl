%% simple_one_for_one supervisor for emongo_server instances
%% there should be one emongo_server_sup instance for each pool, that then
%% supervises emongo_server instances based on the size of pool
-module(emongo_server_sup).

-behaviour(supervisor).

-export([start_link/3, child_count/1, start_child/1, nth_child_pid/2, init/1]).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link(PoolId, Host, Port) ->
	supervisor:start_link({local, PoolId}, ?MODULE, [PoolId, Host, Port]).

child_count(PoolId) -> length(supervisor:which_children(PoolId)).

start_child(PoolId) -> supervisor:start_child(PoolId, []).

nth_child_pid(PoolId, N) ->
	Children = supervisor:which_children(PoolId),
	
	if
		N > length(Children) ->
			throw(badarg);
		true ->
			{undefined, Pid, worker, [emongo_server]} = lists:nth(N, Children),
			Pid
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init([PoolId, Host, Port]) ->
	{ok, {{simple_one_for_one, 10, 10}, [
		{emongo_server, {emongo_server, start_link, [PoolId, Host, Port]},
		 permanent, brutal_kill, worker, [emongo_server]}
	]}}.