%% simple_one_for_one supervisor for emongo_server instances
%% there should be one emongo_server_sup instance for each pool, that then
%% supervises emongo_server instances based on the size of pool
-module(emongo_server_sup).

-behaviour(supervisor).

-export([start_link/3, child_count/1, start_children/3, nth_child_pid/2, init/1]).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link(PoolId, Host, Port) ->
	supervisor:start_link(?MODULE, [PoolId, Host, Port]).

child_count(PoolId) ->
    length(supervisor:which_children(pool_pid(PoolId))).

start_children(_PoolPid, 0, LastPid) ->
    {ok, LastPid};
start_children(PoolPid, Count, _PrevPid) ->
    case supervisor:start_child(PoolPid, []) of
        {ok, Pid} ->
            start_children(PoolPid, Count - 1, Pid);
        Error ->
            Error
    end.

nth_child_pid(PoolId, N) ->
    PoolPid = pool_pid(PoolId),
    Children = supervisor:which_children(PoolPid),
    Missing = N - length(Children),
    
    if
        Missing > 0 ->
            start_children(PoolPid, Missing, undefined);
        true ->
            {undefined, Pid, worker, [emongo_server]} = lists:nth(N, Children),
            {ok, Pid}
    end.

pool_pid(PoolId) ->
    [PoolPid] = [Pid || {Id, Pid, supervisor, _} <- supervisor:which_children(emongo_sup),
                        PoolId =:= Id],
    PoolPid.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init([PoolId, Host, Port]) ->
	{ok, {{simple_one_for_one, 1000, 10}, [
		{emongo_server, {emongo_server, start_link, [PoolId, Host, Port]},
		 permanent, brutal_kill, worker, [emongo_server]}
	]}}.
