#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell -config priv/example

main(_) ->
    etap:plan(unknown),
    error_logger:tty(false),
    etap:ok(application:start(emongo) == ok, "application 'emongo' started ok"),

    emongo:delete(test1, "sushi"),
    etap:is(emongo:find_all(test1, "sushi"), [], "sushi collection is empty"),

    [emongo:insert(test1, "sushi", [{<<"rolls">>, I}]) || I <- lists:seq(1, 50)],

    (fun() ->
             emongo:update(test1, "sushi", [{<<"rolls">>, [{gt, 45}]}], [{<<"$set">>, [{<<"rolls">>, 100}]}], false, true),
             Docs = emongo:find_all(test1, "sushi", [{<<"rolls">>, 100}], []),
             etap:is(length(Docs), 5, "correct number of results after multiupdate"),
             etap:is([I || [_, {_, I}] <- Docs], [100,100,100,100,100], "correct results after multiupdate"),
             ok
     end)(),

    emongo:delete(test1, "sushi"),

    etap:end_tests().
