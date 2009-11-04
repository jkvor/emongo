#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell -config priv/example

main(_) ->
	etap:plan(unknown),
    error_logger:tty(false),
    etap_application:start_ok(emongo, "application 'emongo' started ok"),

	emongo:delete(test1, "sushi"),
	etap:is(emongo:find(test1, "sushi"), [], "sushi collection is empty"),
	
	[emongo:insert(test1, "sushi", [{<<"rolls">>, I}]) || I <- lists:seq(1, 50)],
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"rolls">>, [{gt, 45}]}], [orderby, [{"rolls", asc}]]),
		etap:is(length(Docs), 5, "correct number of results from gt query"),
		etap:is([I || [_, {_, I}] <- Docs], [46,47,48,49,50], "correct results from gt query"),
		ok
	end)(),

	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"rolls">>, [{lt, 5}]}], [orderby, [{"rolls", asc}]]),
		etap:is(length(Docs), 4, "correct number of results from lt query"),
		etap:is([I || [_, {_, I}] <- Docs], [1, 2, 3, 4], "correct results from lt query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"rolls">>, [{gte, 45}]}], [orderby, [{"rolls", asc}]]),
		etap:is(length(Docs), 6, "correct number of results from gte query"),
		etap:is([I || [_, {_, I}] <- Docs], [45,46,47,48,49,50], "correct results from gte query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"rolls">>, [{lte, 5}]}], [orderby, [{"rolls", asc}]]),
		etap:is(length(Docs), 5, "correct number of results from lte query"),
		etap:is([I || [_, {_, I}] <- Docs], [1, 2, 3, 4, 5], "correct results from lte query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"rolls">>, [{ne, 1}, {ne, 50}]}], [orderby, [{"rolls", asc}]]),
		etap:is(length(Docs), 48, "correct number of results from ne query"),
		etap:is([I || [_, {_, I}] <- Docs], lists:seq(2, 49), "correct results from ne query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"rolls">>, [{in, [1,2,3,4,5]}]}], [orderby, [{"rolls", asc}]]),
		etap:is(length(Docs), 5, "correct number of results from in query"),
		etap:is([I || [_, {_, I}] <- Docs], [1,2,3,4,5], "correct results from in query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"rolls">>, [{nin, [1,2,3,4,5]}]}], [orderby, [{"rolls", asc}]]),
		etap:is(length(Docs), 45, "correct number of results from nin query"),
		etap:is([I || [_, {_, I}] <- Docs], lists:seq(6,50), "correct results from nin query"),
		ok
	end)(),
	
	[emongo:insert(test1, "sushi", [{<<"maki">>, {array, [I,I+1,I+2]}}]) || I <- lists:seq(1, 10)],
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"maki">>, [{all, [2,3]}]}], [orderby, [{"maki", asc}]]),
		etap:is(length(Docs), 2, "correct number of results from all query"),
		etap:is([I || [_, {_, I}] <- Docs], [{array, [1,2,3]}, {array, [2,3,4]}], "correct results from all query"),
		ok
	end)(),
	
	emongo:insert(test1, "sushi", [{<<"maki">>, {array, [1,2,3,4,5]}}]),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"maki">>, [{size, 5}]}], [orderby, [{"maki", asc}]]),
		etap:is(length(Docs), 1, "correct number of results from size query"),
		etap:is([I || [_, {_, I}] <- Docs], [{array, [1,2,3,4,5]}], "correct results from size query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"maki">>, [{exists, true}]}], [orderby, [{"maki", asc}]]),
		etap:is(length(Docs), 11, "correct number of results from exists query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{<<"maki">>, [{exists, false}]}], [orderby, [{"maki", asc}]]),
		etap:is(length(Docs), 50, "correct number of results from exists query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{where, "this.rolls > 45"}], [orderby, [{"rolls", asc}]]),
		etap:is(length(Docs), 5, "correct number of results from where query"),
		etap:is([I || [_, {_, I}] <- Docs], [46,47,48,49,50], "correct results from where query"),
		ok
	end)(),
	
	[emongo:insert(test1, "sushi", [{<<"seaweed">>, [{<<"sheets">>, I}]}]) || I <- lists:seq(1,10)],
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{"seaweed.sheets", [{in, [3,4,5]}]}]),
		etap:is(length(Docs), 3, "correct number of results from nested query"),
		etap:is([I || [_, {<<"seaweed">>, [{<<"sheets">>, I}]}] <- Docs], [3,4,5], "correct results from where query"),
		ok
	end)(),
	
   	etap:end_tests().