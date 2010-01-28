-module(emongo_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	{ok, {{one_for_one, 10, 10}, [
		{emongo, {emongo, start_link, []}, permanent, 5000, worker, [emongo]}
	]}}.