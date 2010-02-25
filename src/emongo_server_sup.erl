%% one_for_one supervisor for emongo_server instances
%% there should be one emongo_server_sup instance for each pool, that then
%% supervises emongo_server instances based on the size of pool
-module(emongo_server_sup).

-behaviour(supervisor).

-export([start_link/4, child_count/1, start_child/1, nth_child_pid/2, init/1]).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link(PoolId, Host, Port, Size) ->
    supervisor:start_link(?MODULE, [PoolId, Host, Port, Size]).

child_count(PoolId) ->
    length(supervisor:which_children(pool_pid(PoolId))).

start_child(PoolId) ->
    supervisor:start_child(pool_pid(PoolId), []).

nth_child_pid(PoolId, N) ->
	Children = supervisor:which_children(pool_pid(PoolId)),
	
	if
		N > length(Children) ->
			throw(badarg);
		true ->
			{_, Pid, worker, [emongo_server]} = lists:nth(N, Children),
			Pid
	end.

pool_pid(PoolId) ->
    [PoolPid] = [Pid || {Id, Pid, supervisor, _} <- supervisor:which_children(emongo_sup),
                        PoolId =:= Id],
    PoolPid.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init([PoolId, Host, Port, Size]) ->
    {ok, {{one_for_one, Size * 10, 10},
          [
           {Id,
            {emongo_server, start_link, [PoolId, Host, Port]},
            permanent, brutal_kill, worker, [emongo_server]}
           || Id <- lists:seq(1, Size)
          ]}}.
