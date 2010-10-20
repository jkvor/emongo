%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Jacob Perkins <japerk@gmail.com>
%% Belyaev Dmitry <rumata-estor@nm.ru>
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

-export([pools/0, oid/0, add_pool/5, del_pool/1]).

-export([fold_all/6,
         find_all/2, find_all/3, find_all/4,
         find_one/3, find_one/4]).

-export([insert/3, update/4, update/5, delete/2, delete/3]).

-export([ensure_index/3, count/2, count/3, distinct/3, distinct/4]).

-export([dec2hex/1, hex2dec/1]).

-export([sequence/2, synchronous/0, no_response/0,
         find_all_seq/3, fold_all_seq/5,
         insert_seq/3, update_seq/5, delete_seq/3]).

-export([update_sync/5, delete_sync/3, insert_sync/3]).

-deprecated([update_sync/5, delete_sync/3]).

%% internal
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("emongo.hrl").

-record(state, {oid_index, hashed_hostn}).

%%====================================================================
%% Types
%%====================================================================
%% pool_id() = atom()
%% collection() = string()
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
    emongo_sup:pools().

oid() ->
    gen_server:call(?MODULE, oid, infinity).

add_pool(PoolId, Host, Port, Database, Size) ->
    emongo_sup:start_pool(PoolId, Host, Port, Database, Size).

del_pool(PoolId) ->
    emongo_sup:stop_pool(PoolId).


%%------------------------------------------------------------------------------
%% sequences of operations
%%------------------------------------------------------------------------------

sequence(_PoolId, []) ->
    ok;
sequence(PoolId, Sequence) ->
    {Pid, Database, ReqId} = get_pid_pool(PoolId, length(Sequence)),
    sequence(Sequence, Pid, Database, ReqId).


sequence([Operation|Tail], Pid, Database, ReqId) ->
    Result = Operation(Pid, Database, ReqId),
    case Tail of
        [] -> Result;
        _ -> sequence(Tail, Pid, Database, ReqId + 1)
    end.


synchronous() ->
    synchronous(?TIMEOUT).

synchronous(Timeout) ->
    [fun(_, _, _) -> ok end,
     fun(Pid, Database, ReqId) ->
             PacketGetLastError = emongo_packet:get_last_error(Database, ReqId),
             Resp = emongo_server:send_recv(Pid, ReqId, PacketGetLastError, Timeout),
             Resp#response.documents
     end].

no_response() ->
    [].


%% @spec find(PoolId, Collection, Selector, Options) -> Result
%%   PoolId = atom()
%%   Collection = string()
%%   Selector = document()
%%   Options = [Option]
%%   Option = {timeout, Timeout} | {limit, Limit} | {offset, Offset} | {orderby, Orderby} | {fields, Fields} | explain
%%   Timeout = integer (timeout in milliseconds)
%%   Limit = integer
%%   Offset = integer
%%   Orderby = [{Key, Direction}]
%%   Key = string() | binary() | atom() | integer()
%%   Direction = asc | desc
%%   Fields = [Field]
%%   Field = string() | binary() | atom() | integer() = specifies a field to return in the result set
%%   response_options = return {response, header, response_flag, cursor_id, offset, limit, documents}
%%   Result = documents() | response()

%%------------------------------------------------------------------------------
%% find_all
%%------------------------------------------------------------------------------
find_all(PoolId, Collection) ->
    find_all(PoolId, Collection, [], []).

find_all(PoolId, Collection, Selector) ->
    find_all(PoolId, Collection, Selector, []).

find_all(PoolId, Collection, Selector, Options) ->
    sequence(PoolId, find_all_seq(Collection, Selector, Options)).


find_all_seq(Collection, Selector, Options) ->
    [Fun0,Fun1] = fold_all_seq(fun(I, A) -> [I | A] end, [], Collection, Selector, Options),
    
    [Fun0,
     fun(Pid, Database, ReqId) ->
             lists:reverse(Fun1(Pid, Database, ReqId))
     end].

%%------------------------------------------------------------------------------
%% fold_all
%%------------------------------------------------------------------------------
fold_all(F, Value, PoolId, Collection, Selector, Options) ->
    sequence(PoolId, fold_all_seq(F, Value, Collection, Selector, Options)).


fold_all_seq(F, Value, Collection, Selector, Options) ->
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
    Query = create_query(Options, Selector),
    [fun(_, _, _) -> ok end,
     fun(Pid, Database, ReqId) ->
             Packet = emongo_packet:do_query(Database, Collection, ReqId, Query),
             Resp = emongo_server:send_recv(Pid, ReqId, Packet, Timeout),
             
             NewValue = fold_documents(F, Value, Resp),
             case Query#emo_query.limit of
                 0 ->
                     fold_more(F, NewValue, Collection, Resp#response{documents=[]}, Timeout);
                 _ ->
                     kill_cursor(Resp#response.pool_id, Resp#response.cursor_id),
                     NewValue
             end
     end].


fold_more(_F, Value, _Collection, #response{cursor_id=0}, _Timeout) ->
    Value;

fold_more(F, Value, Collection, #response{pool_id=PoolId, cursor_id=CursorID}, Timeout) ->
    {Pid, Database, ReqId} = get_pid_pool(PoolId, 2),
    Packet = emongo_packet:get_more(Database, Collection, ReqId, 0, CursorID),
    Resp1 = emongo_server:send_recv(Pid, ReqId, Packet, Timeout),

    NewValue = fold_documents(F, Value, Resp1),
    fold_more(F, NewValue, Collection, Resp1#response{documents=[]}, Timeout).

%%------------------------------------------------------------------------------
%% find_one
%%------------------------------------------------------------------------------
find_one(PoolId, Collection, Selector) ->
    find_one(PoolId, Collection, Selector, []).

find_one(PoolId, Collection, Selector, Options) ->
    Options1 = [{limit, 1} | lists:keydelete(limit, 1, Options)],
    find_all(PoolId, Collection, Selector, Options1).

%%------------------------------------------------------------------------------
%% insert
%%------------------------------------------------------------------------------
insert(PoolId, Collection, Documents) ->
    sequence(PoolId, insert_seq(Collection, Documents, no_response())).

insert_seq(Collection, [[_|_]|_]=Documents, Next) ->
    [fun(Pid, Database, ReqId) ->
             Packet = emongo_packet:insert(Database, Collection, ReqId, Documents),
             emongo_server:send(Pid, Packet)
     end | Next];
insert_seq(Collection, Document, Next) ->
    insert_seq(Collection, [Document], Next).


insert_sync(PoolId, Collection, Documents) ->
    sequence(PoolId, insert_seq(Collection, Documents, synchronous())).

%%------------------------------------------------------------------------------
%% update
%%------------------------------------------------------------------------------
update(PoolId, Collection, Selector, Document) ->
    update(PoolId, Collection, Selector, Document, false).

update(PoolId, Collection, Selector, Document, Upsert) ->
    sequence(PoolId, update_seq(Collection, Selector, Document, Upsert, no_response())).


update_seq(Collection, Selector, Document, Upsert, Next) ->
    [fun(Pid, Database, ReqId) ->
             Packet = emongo_packet:update(Database, Collection, ReqId, Upsert, Selector, Document),
             emongo_server:send(Pid, Packet)
     end | Next].


update_sync(PoolId, Collection, Selector, Document, Upsert) ->
    sequence(PoolId, update_seq(Collection, Selector, Document, Upsert, synchronous())).

%%------------------------------------------------------------------------------
%% delete
%%------------------------------------------------------------------------------
delete(PoolId, Collection) ->
    delete(PoolId, Collection, []).

delete(PoolId, Collection, Selector) ->
    sequence(PoolId, delete_seq(Collection, Selector, no_response())).


delete_seq(Collection, Selector, Next) ->
    [fun(Pid, Database, ReqId) ->
             Packet = emongo_packet:delete(Database, Collection, ReqId, transform_selector(Selector)),
             emongo_server:send(Pid, Packet)
     end | Next].


delete_sync(PoolId, Collection, Selector) ->
    sequence(PoolId, delete_seq(Collection, Selector, synchronous())).


%%------------------------------------------------------------------------------
%% ensure index
%%------------------------------------------------------------------------------
%% ensure_index(pool, "collection", [{"fieldname1", 1}, {"fieldname2", -1}]).
ensure_index(PoolId, Collection, Keys) ->
    {Pid, Database, ReqId} = get_pid_pool(PoolId, 1),
    Packet = emongo_packet:ensure_index(Database, Collection, ReqId, Keys),
    emongo_server:send(Pid, Packet).


count(PoolId, Collection) -> count(PoolId, Collection, []).


count(PoolId, Collection, Selector) ->
    {Pid, Database, ReqId} = get_pid_pool(PoolId, 2),
    Q = [{<<"count">>, Collection}, {<<"ns">>, Database},
         {<<"query">>, transform_selector(Selector)}],
    Query = #emo_query{q=Q, limit=1},
    Packet = emongo_packet:do_query(Database, "$cmd", ReqId, Query),
    case emongo_server:send_recv(Pid, ReqId, Packet, ?TIMEOUT) of
        #response{documents=[[{<<"n">>,Count}|_]]} ->
            round(Count);
        _ ->
            undefined
    end.


distinct(PoolId, Collection, Key) -> distinct(PoolId, Collection, Key, []).

distinct(PoolId, Collection, Key, Selector) ->
    {Pid, Database, ReqId} = get_pid_pool(PoolId, 2),
    Q = [{<<"distinct">>, Collection}, {<<"key">>, Key}, {<<"ns">>, Database},
         {<<"query">>, transform_selector(Selector)}],
    Query = #emo_query{q=Q, limit=1},
    Packet = emongo_packet:do_query(Database, "$cmd", ReqId, Query),
    case emongo_server:send_recv(Pid, ReqId, Packet, ?TIMEOUT) of
        #response{documents=[[{<<"values">>, {array, Vals}} | _]]} ->
            Vals;
        _ ->
            undefined
    end.

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
    {ok, HN} = inet:gethostname(),
    <<HashedHN:3/binary,_/binary>> = erlang:md5(HN),
    {ok, #state{oid_index=1, hashed_hostn=HashedHN}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(oid, _From, State) ->
    {Total_Wallclock_Time, _} = erlang:statistics(wall_clock),
    Front = Total_Wallclock_Time rem 16#ffffffff,
    <<_:20/binary,PID:2/binary,_/binary>> = term_to_binary(self()),
    Index = State#state.oid_index rem 16#ffffff,
    {reply, <<Front:32, (State#state.hashed_hostn)/binary, PID/binary, Index:24>>, State#state{oid_index = State#state.oid_index + 1}};

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
get_pid_pool(PoolId, RequestCount) ->
    case emongo_sup:worker_pid(PoolId, emongo_sup:pools(), RequestCount) of
        undefined -> throw(emongo_busy);
        Val -> Val
    end.


fold_documents(F, Value, Resp) ->
    try
        lists:foldl(F, Value, Resp#response.documents)
    catch
        Class:Exception ->
            kill_cursor(Resp#response.pool_id, Resp#response.cursor_id),
            erlang:Class(Exception)
    end.


kill_cursor(_, 0) ->
    ok;
kill_cursor(PoolId, CursorID)  ->
    {Pid, _Database, ReqId} = get_pid_pool(PoolId, 1),
    Packet = emongo_packet:kill_cursors(ReqId, [CursorID]),
    emongo_server:send(Pid, ReqId, Packet).


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
    QueryRec#emo_query{q=(OptDoc ++ [{<<"$query">>, [{none, none}]}])};

create_query([], QueryRec, QueryDoc, OptDoc) ->
    QueryRec#emo_query{q=(OptDoc ++ [{<<"$query">>, QueryDoc}])};

create_query([{limit, Limit}|Options], QueryRec, QueryDoc, OptDoc) ->
    QueryRec1 = QueryRec#emo_query{limit=Limit},
    create_query(Options, QueryRec1, QueryDoc, OptDoc);

create_query([{offset, Offset}|Options], QueryRec, QueryDoc, OptDoc) ->
    QueryRec1 = QueryRec#emo_query{offset=Offset},
    create_query(Options, QueryRec1, QueryDoc, OptDoc);

create_query([{orderby, Orderby}|Options], QueryRec, QueryDoc, OptDoc) ->
    Orderby1 = [{Key, case Dir of desc -> -1; _ -> 1 end}|| {Key, Dir} <- Orderby],
    OptDoc1 = [{<<"$orderby">>, Orderby1}|OptDoc],
    create_query(Options, QueryRec, QueryDoc, OptDoc1);

create_query([{fields, Fields}|Options], QueryRec, QueryDoc, OptDoc) ->
    QueryRec1 = QueryRec#emo_query{field_selector=[{Field, 1} || Field <- Fields]},
    create_query(Options, QueryRec1, QueryDoc, OptDoc);

create_query([explain | Options], QueryRec, QueryDoc, OptDoc) ->
    create_query(Options, QueryRec, QueryDoc, [{<<"$explain">>,true}|OptDoc]);

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

dec0($a) -> 10;
dec0($b) -> 11;
dec0($c) -> 12;
dec0($d) -> 13;
dec0($e) -> 14;
dec0($f) -> 15;
dec0(X) -> X - $0.

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 + I.
