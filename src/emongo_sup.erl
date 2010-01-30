-module(emongo_sup).

-behaviour(supervisor).

-export([start_link/0, start_pool/3, init/1]).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_pool(PoolId, Host, Port) ->
	% emongo_server_sup instances are added dynamically, one for each pool
	supervisor:start_child(?MODULE, {PoolId,
		{emongo_server_sup, start_link, [PoolId, Host, Port]},
		permanent, infinity, supervisor, [emongo_server_sup]
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
	{ok, {{one_for_one, 10, 10}, [
		{emongo, {emongo, start_link, []},
		 permanent, 5000, worker, [emongo]}
	]}}.