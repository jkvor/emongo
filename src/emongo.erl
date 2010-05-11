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

-export([pools/0, oid/0, add_pool/5, find/2, find/3, find/4,
		 find_all/2, find_all/3, find_all/4, get_more/4,
		 get_more/5, find_one/3, find_one/4, kill_cursors/2,
		 insert/3, update/4, update/5, delete/2, delete/3,
		 ensure_index/3, count/2, count/3, distinct/3, distinct/4,
		 dec2hex/1, hex2dec/1]).

-include("emongo.hrl").

-record(state, {pools, oid_index, hashed_hostn}).

%%====================================================================
%% Types
%%====================================================================
%% pool_id() = atom()
%% collection() = string()
%% response() = {response, header, response_flag, cursor_id, offset, limit, documents}
%% documents() = [document()]
%% document() = [{term(), term()}]

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

oid() ->
	gen_server:call(?MODULE, oid, infinity).

add_pool(PoolId, Host, Port, Database, Size) ->
	gen_server:call(?MODULE, {add_pool, PoolId, Host, Port, Database, Size}, infinity).

%%------------------------------------------------------------------------------
%% find
%%------------------------------------------------------------------------------
find(PoolId, Collection) ->
	find(PoolId, Collection, [], [{timeout, ?TIMEOUT}]).

find(PoolId, Collection, Selector) when ?IS_DOCUMENT(Selector) ->
	find(PoolId, Collection, Selector, [{timeout, ?TIMEOUT}]);

%% this function has been deprecated
find(PoolId, Collection, Query) when is_record(Query, emo_query) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:do_query(Pool#pool.database, Collection, Pool#pool.req_id, Query),
	emongo_server:send_recv(Pid, Pool#pool.req_id, Packet, ?TIMEOUT).

%% @spec find(PoolId, Collection, Selector, Options) -> Result
%%		 PoolId = atom()
%%		 Collection = string()
%%		 Selector = document()
%%		 Options = [Option]
%%		 Option = {timeout, Timeout} | {limit, Limit} | {offset, Offset} | {orderby, Orderby} | {fields, Fields} | response_options
%%		 Timeout = integer (timeout in milliseconds)
%%		 Limit = integer
%%		 Offset = integer
%%		 Orderby = [{Key, Direction}]
%%		 Key = string() | binary() | atom() | integer()
%%		 Direction = asc | desc
%%		 Fields = [Field]
%%		 Field = string() | binary() | atom() | integer() = specifies a field to return in the result set
%%		 response_options = return {response, header, response_flag, cursor_id, offset, limit, documents}
%%		 Result = documents() | response()
find(PoolId, Collection, Selector, Options) when ?IS_DOCUMENT(Selector), is_list(Options) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Query = create_query(Options, Selector),
	Packet = emongo_packet:do_query(Pool#pool.database, Collection, Pool#pool.req_id, Query),
	Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
	% TODO: generalize this for all send_recv calls
	try emongo_server:send_recv(Pid, Pool#pool.req_id, Packet, Timeout) of
		Resp ->
			case lists:member(response_options, Options) of
				true -> Resp;
				false -> Resp#response.documents
			end
	catch
		exit:{timeout, Reason} ->
			% force restart of affected emongo_server and try again with a new
			% emongo_server pid
			error_logger:warning_report([{timeout, Pid}]),
			exit(Pid, Reason),
			find(PoolId, Collection, Selector, Options)
	end.

%%------------------------------------------------------------------------------
%% find_all
%%------------------------------------------------------------------------------
find_all(PoolId, Collection) ->
	find_all(PoolId, Collection, [], [{timeout, ?TIMEOUT}]).

find_all(PoolId, Collection, Selector) when ?IS_DOCUMENT(Selector) ->
	find_all(PoolId, Collection, Selector, [{timeout, ?TIMEOUT}]).

find_all(PoolId, Collection, Selector, Options) when ?IS_DOCUMENT(Selector), is_list(Options) ->
	Resp = find(PoolId, Collection, Selector, [response_options|Options]),
	find_all(PoolId, Collection, Selector, Options, Resp).

find_all(_PoolId, _Collection, _Selector, Options, Resp) when is_record(Resp, response), Resp#response.cursor_id == 0 ->
	case lists:member(response_options, Options) of
		true -> Resp;
		false -> Resp#response.documents
	end;

find_all(PoolId, Collection, Selector, Options, Resp) when is_record(Resp, response) ->
	Resp1 = get_more(PoolId, Collection, Resp#response.cursor_id, proplists:get_value(timeout, Options, ?TIMEOUT)),
	Documents = lists:append(Resp#response.documents, Resp1#response.documents),
	find_all(PoolId, Collection, Selector, Options, Resp1#response{documents=Documents}).

%%------------------------------------------------------------------------------
%% find_one
%%------------------------------------------------------------------------------
find_one(PoolId, Collection, Selector) when ?IS_DOCUMENT(Selector) ->
	find_one(PoolId, Collection, Selector, [{timeout, ?TIMEOUT}]).

find_one(PoolId, Collection, Selector, Options) when ?IS_DOCUMENT(Selector), is_list(Options) ->
	Options1 = [{limit, 1} | lists:delete(limit, Options)],
	find(PoolId, Collection, Selector, Options1).

%%------------------------------------------------------------------------------
%% get_more
%%------------------------------------------------------------------------------
get_more(PoolId, Collection, CursorID, Timeout) ->
	get_more(PoolId, Collection, CursorID, 0, Timeout).

get_more(PoolId, Collection, CursorID, NumToReturn, Timeout) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:get_more(Pool#pool.database, Collection, Pool#pool.req_id, NumToReturn, CursorID),
	emongo_server:send_recv(Pid, Pool#pool.req_id, Packet, Timeout).

kill_cursors(PoolId, CursorID) when is_integer(CursorID) ->
	kill_cursors(PoolId, [CursorID]);

kill_cursors(PoolId, CursorIDs) when is_list(CursorIDs) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:kill_cursors(Pool#pool.req_id, CursorIDs),
	emongo_server:send(Pid, Pool#pool.req_id, Packet).

%%------------------------------------------------------------------------------
%% insert
%%------------------------------------------------------------------------------
insert(PoolId, Collection, Document) when ?IS_DOCUMENT(Document) ->
	insert(PoolId, Collection, [Document]);

insert(PoolId, Collection, Documents) when ?IS_LIST_OF_DOCUMENTS(Documents) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:insert(Pool#pool.database, Collection, Pool#pool.req_id, Documents),
	emongo_server:send(Pid, Pool#pool.req_id, Packet).

%%------------------------------------------------------------------------------
%% update
%%------------------------------------------------------------------------------
update(PoolId, Collection, Selector, Document) when ?IS_DOCUMENT(Selector), ?IS_DOCUMENT(Document) ->
	update(PoolId, Collection, Selector, Document, false).

update(PoolId, Collection, Selector, Document, Upsert) when ?IS_DOCUMENT(Selector), ?IS_DOCUMENT(Document) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:update(Pool#pool.database, Collection, Pool#pool.req_id, Upsert, Selector, Document),
	emongo_server:send(Pid, Pool#pool.req_id, Packet).

%%------------------------------------------------------------------------------
%% delete
%%------------------------------------------------------------------------------
delete(PoolId, Collection) ->
	delete(PoolId, Collection, []).

delete(PoolId, Collection, Selector) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:delete(Pool#pool.database, Collection, Pool#pool.req_id, transform_selector(Selector)),
	emongo_server:send(Pid, Pool#pool.req_id, Packet).

%%------------------------------------------------------------------------------
%% ensure index
%%------------------------------------------------------------------------------
ensure_index(PoolId, Collection, Keys) when ?IS_DOCUMENT(Keys)->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Packet = emongo_packet:ensure_index(Pool#pool.database, Collection, Pool#pool.req_id, Keys),
	emongo_server:send(Pid, Pool#pool.req_id, Packet).

count(PoolId, Collection) -> count(PoolId, Collection, []).

count(PoolId, Collection, Selector) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Q = [{<<"count">>, Collection}, {<<"ns">>, Pool#pool.database},
		 {<<"query">>, transform_selector(Selector)}],
	Query = #emo_query{q=Q, limit=1},
	Packet = emongo_packet:do_query(Pool#pool.database, "$cmd", Pool#pool.req_id, Query),
	case emongo_server:send_recv(Pid, Pool#pool.req_id, Packet, ?TIMEOUT) of
		#response{documents=[[{<<"n">>,Count}|_]]} ->
			round(Count);
		_ ->
			undefined
	end.

distinct(PoolId, Collection, Key) -> distinct(PoolId, Collection, Key, []).

distinct(PoolId, Collection, Key, Selector) ->
	{Pid, Pool} = gen_server:call(?MODULE, {pid, PoolId}, infinity),
	Q = [{<<"distinct">>, Collection}, {<<"key">>, Key}, {<<"ns">>, Pool#pool.database},
		 {<<"query">>, transform_selector(Selector)}],
	Query = #emo_query{q=Q, limit=1},
	Packet = emongo_packet:do_query(Pool#pool.database, "$cmd", Pool#pool.req_id, Query),
	case emongo_server:send_recv(Pid, Pool#pool.req_id, Packet, ?TIMEOUT) of
		#response{documents=[[{<<"values">>, {array, Vals}} | _]]} ->
			Vals;
		_ ->
			undefined
	end.

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
	%Pools = initialize_pools(),
	{ok, HN} = inet:gethostname(),
	<<HashedHN:3/binary,_/binary>> = erlang:md5(HN),
	{ok, #state{pools=[], oid_index=1, hashed_hostn=HashedHN}}.

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

handle_call(oid, _From, State) ->
	{Total_Wallclock_Time, _} = erlang:statistics(wall_clock),
	Front = Total_Wallclock_Time rem 16#ffffffff,
	<<_:20/binary,PID:2/binary,_/binary>> = term_to_binary(self()),
	Index = State#state.oid_index rem 16#ffffff,
	{reply, <<Front:32, (State#state.hashed_hostn)/binary, PID/binary, Index:24>>, State#state{oid_index = State#state.oid_index + 1}};

handle_call({add_pool, PoolId, Host, Port, Database, Size}, _From, #state{pools=Pools}=State) ->
	case proplists:get_value(PoolId, Pools) of
		undefined ->
			Pool = #pool{
				id=PoolId,
				host=Host,
				port=Port,
				database=Database,
				size=Size
			},
			emongo_sup:start_pool(PoolId, Host, Port),
			Pool1 = do_open_connections(Pool),
			Pools1 = [{PoolId, Pool1} | Pools];
		Pool ->
			Pool1 = do_open_connections(Pool),
			Pools1 = [{PoolId, Pool1} | proplists:delete(PoolId, Pools)]
	end,
	
	{reply, ok, State#state{pools=Pools1}};

handle_call({pid, PoolId1}, From, #state{pools=[]}=State) ->
	F = fun({PoolId, Props}, State1) ->
			Host = proplists:get_value(host, Props, "localhost"),
			Port = proplists:get_value(port, Props, 27017),
			Database = proplists:get_value(database, Props, "test"),
			Size = proplists:get_value(size, Props, 1),
			Req = {add_pool, PoolId, Host, Port, Database, Size},
			{reply, ok, State2} = handle_call(Req, From, State1),
			State2
		end,
	
	case application:get_env(emongo, pools) of
		undefined ->
			{reply, {undefined, undefined}, State};
		{ok, []} ->
			{reply, {undefined, undefined}, State};
		{ok, Pools} ->
			NewState = lists:foldl(F, State, Pools),
			handle_call({pid, PoolId1}, From, NewState)
	end;
handle_call({pid, PoolId}, _From, #state{pools=Pools}=State) ->
	case lists:keytake(PoolId, 1, Pools) of
		false ->
			{reply, {undefined, undefined}, State};
		{value, {PoolId, Pool}, Others} ->
			N = Pool#pool.conn_pid,
			Pid = emongo_server_sup:nth_child_pid(Pool#pool.id, N),
			
			if
				N < Pool#pool.size -> N1 = N + 1;
				true -> N1 = 1
			end,
			
			Pool1 = Pool#pool{conn_pid=N1, req_id=(Pool#pool.req_id)+1},
			Pools1 = [{PoolId, Pool1} | Others],
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
handle_info({'EXIT', _Pid, {PoolId, tcp_closed}}, #state{pools=Pools}=State) ->
	case lists:keytake(PoolId, 1, Pools) of
		false ->
			{noreply, State};
		{value, {PoolId, Pool}, Others} ->
			Pool1 = do_open_connections(Pool),
			{noreply, State#state{pools=[{PoolId, Pool1} | Others]}}
	end;

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

do_open_connections(#pool{size=Size}=Pool) ->
	% each connection is an emongo_server supervised by simple_one_for_one
	% emongo_server_sup supervisor
	F = fun(_) -> catch emongo_server_sup:start_child(Pool#pool.id) end,
	
	case Size - emongo_server_sup:child_count(Pool#pool.id) of
		I when I < 1 -> ok;
		I -> lists:foreach(F, lists:seq(1, I))
	end,
	
	Pool.

dec2hex(Dec) ->
	dec2hex(<<>>, Dec).

dec2hex(N, <<I:8,Rem/binary>>) ->
	dec2hex(<<N/binary, (hex0((I band 16#f0) bsr 4)):8, (hex0((I band 16#0f))):8>>, Rem);
dec2hex(N,<<>>) ->
	N.

hex2dec(Hex) when is_list(Hex) ->
	hex2dec(list_to_binary(Hex));

hex2dec(Hex) ->
	hex2dec(<<>>, Hex).

hex2dec(N,<<A:8,B:8,Rem/binary>>) ->
	hex2dec(<<N/binary, ((dec0(A) bsl 4) + dec0(B)):8>>, Rem);
hex2dec(N,<<>>) ->
	N.

create_query(Options, Selector) ->
	Selector1 = transform_selector(Selector),
	create_query(Options, #emo_query{}, Selector1, []).

create_query([], QueryRec, QueryDoc, []) ->
	QueryRec#emo_query{q=QueryDoc};

create_query([], QueryRec, [], OptDoc) ->
	QueryRec#emo_query{q=OptDoc};

create_query([], QueryRec, QueryDoc, OptDoc) ->
	QueryRec#emo_query{q=(OptDoc ++ [{<<"query">>, QueryDoc}])};

create_query([{limit, Limit}|Options], QueryRec, QueryDoc, OptDoc) ->
	QueryRec1 = QueryRec#emo_query{limit=Limit},
	create_query(Options, QueryRec1, QueryDoc, OptDoc);

create_query([{offset, Offset}|Options], QueryRec, QueryDoc, OptDoc) ->
	QueryRec1 = QueryRec#emo_query{offset=Offset},
	create_query(Options, QueryRec1, QueryDoc, OptDoc);

create_query([{orderby, Orderby}|Options], QueryRec, QueryDoc, OptDoc) ->
	Orderby1 = [{Key, case Dir of desc -> -1; _ -> 1 end}|| {Key, Dir} <- Orderby],
	OptDoc1 = [{<<"orderby">>, Orderby1}|OptDoc],
	create_query(Options, QueryRec, QueryDoc, OptDoc1);

create_query([{fields, Fields}|Options], QueryRec, QueryDoc, OptDoc) ->
	QueryRec1 = QueryRec#emo_query{field_selector=[{Field, 1} || Field <- Fields]},
	create_query(Options, QueryRec1, QueryDoc, OptDoc);

create_query([_|Options], QueryRec, QueryDoc, OptDoc) ->
	create_query(Options, QueryRec, QueryDoc, OptDoc).

transform_selector(Selector) ->
	transform_selector(Selector, []).

transform_selector([], Acc) ->
	lists:reverse(Acc);

transform_selector([{where, Val}|Tail], Acc) when is_list(Val) ->
	transform_selector(Tail, [{<<"$where">>, Val}|Acc]);

transform_selector([{Key, [{_,_}|_]=Vals}|Tail], Acc) ->
	Vals1 =
		[case Operator of
			O when O == '>'; O == gt ->
				{<<"$gt">>, Val};
			O when O == '<'; O == lt ->
				{<<"$lt">>, Val};
			O when O == '>='; O == gte ->
				{<<"$gte">>, Val};
			O when O == '=<'; O == lte ->
				{<<"$lte">>, Val};
			O when O == '=/='; O == '/='; O == ne ->
				{<<"$ne">>, Val};
			in when is_list(Val) ->
				{<<"$in">>, {array, Val}};
			nin when is_list(Val) ->
				{<<"$nin">>, {array, Val}};
			mod when is_list(Val), length(Val) == 2 ->
				{<<"$mod">>, {array, Val}};
			all when is_list(Val) ->
				{<<"$all">>, {array, Val}};
			size when is_integer(Val) ->
				{<<"$size">>, Val};
			exists when is_boolean(Val) ->
				{<<"$exists">>, Val};
			_ ->
				{Operator, Val}
		 end || {Operator, Val} <- Vals],
	transform_selector(Tail, [{Key, Vals1}|Acc]);

transform_selector([Other|Tail], Acc) ->
	transform_selector(Tail, [Other|Acc]).

dec0($a) ->	10;
dec0($b) ->	11;
dec0($c) ->	12;
dec0($d) ->	13;
dec0($e) ->	14;
dec0($f) ->	15;
dec0(X) ->	X - $0.

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 + I.
