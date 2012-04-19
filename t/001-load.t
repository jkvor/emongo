#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell -config priv/example

main(_) ->
	etap:plan(unknown),
    error_logger:tty(false),
    etap_application:start_ok(emongo, "application 'emongo' started ok"),

	etap:is(length(emongo:pools()), 1, "one pool exists in state"),

   	etap:end_tests().