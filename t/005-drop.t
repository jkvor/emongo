#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell -config priv/example

main(_) ->
    etap:plan(unknown),
    error_logger:tty(false),
    etap:ok(application:start(emongo) == ok, "application 'emongo' started ok"),

    [emongo:insert(test1, "sushi", [{<<"rolls">>, I}]) || I <- lists:seq(1, 50)],
    [emongo:insert(test1, "sushi2", [{<<"rolls">>, I}]) || I <- lists:seq(1, 50)],

    ok = emongo:drop_database(test1),

    emongo:insert(test1, "sushi", [{<<"rolls">>, 1}]),
    etap:is(length(emongo:find_all(test1, "sushi")), 1, "There is 1 doc"),
    etap:is(length(emongo:find_all(test1, "sushi2")), 0, "there is 0 doc"),

    emongo:delete(test1, "sushi"),

    etap:end_tests().
