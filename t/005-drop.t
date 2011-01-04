#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell -config priv/example

main(_) ->
    etap:plan(unknown),
    error_logger:tty(false),
    etap_application:start_ok(emongo, "application 'emongo' started ok"),

    [emongo:insert(test1, "sushi", [{<<"rolls">>, I}]) || I <- lists:seq(1, 50)],
    [emongo:insert(test1, "sushi2", [{<<"rolls">>, I}]) || I <- lists:seq(1, 50)],

    ok = emongo:drop_database(test1),

    emongo:insert(test1, "sushi", [{<<"rolls">>, 1}]),
    [_] = emongo:find_all(test1, "sushi"),
    [] = emongo:find_all(test1, "sushi2"),

     etap:end_tests().
