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
-module(emongo).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

-export([pools/0, add_pool/5, find/2, find/3, find/4, 
		 find_one/3, find_one/4, insert/3]).

-include("emongo.hrl").

-record(state, {pools}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
		
pools() ->
	gen_server:call(?MODULE, pools, infinity).
	
add_pool(PoolId, Host, Port, Database, Size) ->
	gen_server:call(?MODULE, {add_pool, PoolId, Host, Port, Database, Size}, infinity).
	
%%show_dbs() -> ok.

%%show_collections(Database) -> ok.

%%show_users(Database) -> ok.

%%show_profile(Database) -> ok.

%%use_db(PoolId) -> ok.

find(PoolId, Collection) ->
	find(PoolId, Collection, [], ?TIMEOUT).

find(PoolId, Collection, Document) ->
	find(PoolId, Collection, Document, ?TIMEOUT).
	
find(PoolId, Collection, Document, Timeout) when is_list(Document) ->
	find(PoolId, Collection, #emo_query{q=Document}, Timeout);
	
find(PoolId, Collection, {oid, OID}, Timeout) when is_binary(OID) ->
	find(PoolId, Collection, #emo_query{q=[{"_id", {oid, OID}}], limit=1}, Timeout);
	
find(PoolId, Collection, Query, Timeout) when is_record(Query, emo_query) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:do_query(Pool#pool.database, Collection, Pool#pool.req_id, Query),
	io:format("packet ~p~n", [Packet]),
	emongo_conn:send_recv(Pid, Pool#pool.req_id, Packet, Timeout).
	
find_one(PoolId, Collection, Document) ->
	find_one(PoolId, Collection, Document, ?TIMEOUT).

find_one(PoolId, Collection, Document, Timeout) when is_list(Document) ->
	find(PoolId, Collection, #emo_query{q=Document, limit=1}, Timeout);

find_one(PoolId, Collection, {oid, OID}, Timeout) when is_binary(OID) ->
	find(PoolId, Collection, #emo_query{q=[{"_id", {oid, OID}}], limit=1}, Timeout).

insert(PoolId, Collection, Document) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:insert(Pool#pool.database, Collection, Pool#pool.req_id, Document),
	emongo_conn:send(Pid, Pool#pool.req_id, Packet).

%%update

%save(PoolId, {obj, Props}) -> ok.

%remove(PoolId, {obj, Props}) -> ok.

%%ensure_index

%%count

%drop_collection(PoolId, Collection) when is_atom(PoolId), is_list(Collection) ->

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
init(_) ->
	process_flag(trap_exit, true),
	Pools = initialize_pools(),
	{ok, #state{pools=Pools}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(pools, _From, State) ->
	{reply, State#state.pools, State};
	
handle_call({add_pool, PoolId, Host, Port, Database, Size}, _From, #state{pools=Pools}=State) ->
	{Result, Pools1} = 
		case proplists:is_defined(PoolId, Pools) of
			true ->
				{{error, pool_already_exists}, Pools};
			false ->
				Pool = #pool{
					id=PoolId,
					host=Host,
					port=Port,
					database=Database,
					size=Size
				},
				Pool1 = do_open_connections(Pool),
				{ok, [{PoolId, Pool1}|Pools]}
		end,
	{reply, Result, State#state{pools=Pools1}};
	
handle_call({pid, PoolId}, _From, #state{pools=Pools}=State) ->
	case get_pool(PoolId, Pools) of
		undefined ->
			{reply, {undefined, undefined}, State};
		{Pool, Others} ->
			Pid = get_conn_pid(Pool),
			Pool1 = Pool#pool{req_id = ((Pool#pool.req_id)+1)},
			Pools1 = [{PoolId, Pool1}|Others],
			{reply, {Pid, Pool}, State#state{pools=Pools1}}
	end;
	
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

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
handle_info({'EXIT', Pid, {PoolId, tcp_closed}}, #state{pools=Pools}=State) ->
	State1 =
		case get_pool(PoolId, Pools) of
			undefined ->
				State;
			{Pool, Others} ->
				Pids1 = lists:delete(Pid, Pool#pool.conn_pids),
				Pool1 = Pool#pool{conn_pids = Pids1},
				Pool2 = do_open_connections(Pool1),
				Pools1 = [{PoolId, Pool2}|Others],
				State#state{pools=Pools1}
		end,
	{noreply, State1};
	
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
initialize_pools() ->
	case application:get_env(emongo, pools) of
		undefined ->
			[];
		{ok, Pools} ->
			[begin
				Pool = #pool{
					id = PoolId, 
					size = proplists:get_value(size, Props, 1),
					host = proplists:get_value(host, Props, "localhost"), 
					port = proplists:get_value(port, Props, 27017), 
					database = proplists:get_value(database, Props, "test")
				},
				{PoolId, do_open_connections(Pool)}
			 end || {PoolId, Props} <- Pools]
	end.
		
do_open_connections(#pool{conn_pids=Pids, size=Size}=Pool) when length(Pids) < Size -> 
	Pid = emongo_conn:start_link(Pool#pool.id, Pool#pool.host, Pool#pool.port),
	do_open_connections(Pool#pool{conn_pids = [Pid|Pids]});
	
do_open_connections(Pool) -> 
	Pool.

get_conn_pid(#pool{conn_pids=[Pid|_]}) -> Pid.

get_pool(PoolId, Pools) ->
	get_pool(PoolId, Pools, []).
	
get_pool(_, [], _) ->
	undefined;
		
get_pool(PoolId, [{PoolId, Pool}|Tail], Others) ->
	{Pool, lists:append(Tail, Others)};
	
get_pool(PoolId, [Pool|Tail], Others) ->
	get_pool(PoolId, Tail, [Pool|Others]).
	
	
	
	
	
	
	