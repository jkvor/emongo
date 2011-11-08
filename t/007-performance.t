#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell -config priv/example

-define(NUM_PROCESSES,     500).
-define(NUM_TESTS_PER_PID, 10).
-define(POOL,              test2).
-define(COLL,              <<"sushi">>).
-define(TIMEOUT,           10000).
-define(OUT(Format, Data), io:format(Format ++ "\n", Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(_) ->
    etap:plan(unknown),
    error_logger:tty(false),
    etap:ok(application:start(emongo) == ok, "application 'emongo' started ok"),
    emongo:delete(?POOL, ?COLL, []),
    (fun() ->
             test_performance()
     end)(),
    emongo:delete(?POOL, ?COLL, []),
    etap:end_tests().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_performance() ->
  ?OUT("Testing performance", []),
  Start = cur_time_ms(),
  try
    start_processes(?NUM_PROCESSES),
    block_until_done(?NUM_PROCESSES)
  after
    % Clean up in case something failed.
    emongo:delete_sync(?POOL, ?COLL, [])
  end,
  End = cur_time_ms(),
  ?OUT("Test passed in ~p ms\n", [End - Start]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    IRes = emongo:insert_sync(?POOL, ?COLL, [{"_id", Num}]),
    ok = check_result(IRes),
    %etap:is(check_result(IRes), ok, "insert_sync ok"),

    FMRes = emongo:find_and_modify(?POOL, ?COLL, [{"_id", Num}],
      [{<<"$set">>, [{<<"fm">>, Num}]}], [{new, true}]),
    [[{<<"value">>, [{<<"_id">>, Num}, {<<"fm">>, Num}]}, {<<"ok">>, 1.0}]] =
      FMRes,
    %etap:is(FMRes, [[{<<"value">>, [{<<"_id">>, Num}, {<<"fm">>, Num}]},
    %                 {<<"ok">>, 1.0}]], "find_all ok"),

    URes = emongo:update_sync(?POOL, ?COLL, [{"_id", Num}],
      [{<<"$set">>, [{<<"us">>, Num}]}], false),
    ok = check_result(URes),
    %etap:is(check_result(URes), ok, "update_sync ok"),

    FARes = emongo:find_all(?POOL, ?COLL, [{"_id", Num}]),
    [[{<<"_id">>, Num}, {<<"fm">>, Num}, {<<"us">>, Num}]] = FARes,
    %etap:is(FARes, [[{<<"_id">>, Num}, {<<"fm">>, Num}, {<<"us">>, Num}]],
    %        "find_all ok"),

    DRes = emongo:delete_sync(?POOL, ?COLL, [{"_id", Num}]),
    ok = check_result(DRes)
    %etap:is(check_result(DRes), ok, "delete_sync ok")
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

check_result([List]) when is_list(List) -> check_result_int(List);
check_result(_)                         -> {error, invalid_result}.

check_result_int([])                       -> {error, result_unknown};
check_result_int([{<<"err">>, null} | _])  -> ok;
check_result_int([{<<"err">>, Error} | _]) -> {error, Error};
check_result_int([_ | Rest])               -> check_result_int(Rest).
