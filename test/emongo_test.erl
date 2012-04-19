-module(emongo_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(NUM_PROCESSES,     500).
-define(NUM_TESTS_PER_PID, 10).
-define(POOL,              pool1).
-define(COLL,              <<"test">>).
-define(TIMEOUT,           5000).
-define(OUT(F, D),         ?debugFmt(F, D)).

setup() ->
  ensure_started(sasl),
  ensure_started(emongo),
  emongo:add_pool(?POOL, "localhost", 27017, "testdatabase", 10),
  ok.

cleanup(_) ->
  ok.

run_test_() ->
  [{setup,
    fun setup/0,
    fun cleanup/1,
    [
      fun test_performance/0
    ]
  }].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_performance() ->
  ?OUT("Testing performance.", []),
  emongo:delete_sync(?POOL, ?COLL),
  Start = cur_time_ms(),
  try
    start_processes(?NUM_PROCESSES),
    block_until_done(?NUM_PROCESSES)
  after
    % Clean up in case something failed.
    emongo:delete_sync(?POOL, ?COLL)
  end,
  End = cur_time_ms(),
  ?OUT("Test passed in ~p ms\n", [End - Start]).

start_processes(X) when X =< 0 -> ok;
start_processes(X) ->
  Pid = self(),
  proc_lib:spawn(fun() ->
    run_tests(Pid, X, ?NUM_TESTS_PER_PID)
  end),
  start_processes(X - 1).

run_tests(Pid, _, Y) when Y =< 0 ->
  Pid ! done;
run_tests(Pid, X, Y) ->
  Num = (X bsl 16) bor Y, % Make up a unique number for this run
  try
    IRes = emongo:insert_sync(?POOL, ?COLL, [{"_id", Num}], [response_options]),
    ok = check_result("insert_sync", IRes, 0),

    [FMRes] = emongo:find_and_modify(?POOL, ?COLL, [{"_id", Num}],
      [{<<"$set">>, [{<<"fm">>, Num}]}], [{new, true}]),
    FMVal = proplists:get_value(<<"value">>, FMRes),
    [{<<"_id">>, Num}, {<<"fm">>, Num}] = FMVal,

    URes = emongo:update_sync(?POOL, ?COLL, [{"_id", Num}],
      [{<<"$set">>, [{<<"us">>, Num}]}], false, [response_options]),
    ok = check_result("update_sync", URes, 1),

    FARes = emongo:find_all(?POOL, ?COLL, [{"_id", Num}]),
    [[{<<"_id">>, Num}, {<<"fm">>, Num}, {<<"us">>, Num}]] = FARes,

    DRes = emongo:delete_sync(?POOL, ?COLL, [{"_id", Num}], [response_options]),
    ok = check_result("delete_sync", DRes, 1)
  catch _:E ->
    ?OUT("Exception occurred for test ~.16b: ~p\n~p\n",
              [Num, E, erlang:get_stacktrace()]),
    throw(test_failed)
  end,
  run_tests(Pid, X, Y - 1).

block_until_done(X) when X =< 0 -> ok;
block_until_done(X) ->
  receive done -> ok
  after ?TIMEOUT ->
    ?OUT("No response\n", []),
    throw(test_failed)
  end,
  block_until_done(X - 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

cur_time_ms() ->
  {MegaSec, Sec, MicroSec} = erlang:now(),
  MegaSec * 1000000000 + Sec * 1000 + erlang:round(MicroSec / 1000).

check_result(Desc,
             {response, _,_,_,_,_, [List]},
             ExpectedN) when is_list(List) ->
  {_, Err} = lists:keyfind(<<"err">>, 1, List),
  {_, N}   = lists:keyfind(<<"n">>,   1, List),
  if Err == undefined, N == ExpectedN -> ok;
  true ->
    ?OUT("Unexpected result for ~p: Err = ~p; N = ~p", [Desc, Err, N]),
    throw({error, invalid_db_response})
  end.
