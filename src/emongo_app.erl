%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(emongo_app).

-behaviour(application).

-include("emongo.hrl").

-export([start/2, stop/1, initialize_pools/0]).

start(_, _) ->
	{ok, Pid} = emongo_sup:start_link(),
	% Pools must be initialized after emongo_sup is started instead of in
	% emongo:init, because emongo_server_sup instances are dynamically added
	% to the emongo_sup supervisor, which also supervises emongo gen_server.
	% (otherwise get a deadlock where emongo is waiting on emongo_sup, which
	% is waiting on emongo)
	initialize_pools(),
	{ok, Pid}.

stop(_) -> ok.

initialize_pools() ->
	F = fun({PoolId, Props}) ->
			Host = proplists:get_value(host, Props, "localhost"),
			Port = proplists:get_value(port, Props, 27017),
			Database = proplists:get_value(database, Props, "test"),
			Size = proplists:get_value(size, Props, 1),
			emongo:add_pool(PoolId, Host, Port, Database, Size)
		end,
	
	case application:get_env(emongo, pools) of
		undefined -> ok;
		{ok, Pools} -> lists:foreach(F, Pools)
	end.