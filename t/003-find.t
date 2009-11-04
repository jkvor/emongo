#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell -config priv/example

main(_) ->
	etap:plan(unknown),
    error_logger:tty(false),
    etap_application:start_ok(emongo, "application 'emongo' started ok"),

	emongo:delete(test1, "sushi"),
	etap:is(emongo:find(test1, "sushi"), [], "sushi collection is empty"),
	
	[emongo:insert(test1, "sushi", [{<<"rolls">>, I}, {<<"uni">>, <<"nigiri">>}]) || I <- lists:seq(1, 1000)],
	
	All = emongo:find_all(test1, "sushi", [], [{orderby, [{"rolls", asc}]}]),
	etap:is(length(All), 1000, "inserted documents into sushi collection"),
	
	(fun() ->
		OrderOK = lists:foldl(
			fun(Item, I) ->
				case Item of
					[_, {<<"rolls">>, I}, _] -> I + 1;
					_ -> -1
				end
			end, 1, All),
		etap:is(OrderOK, 1001, "sushi collection returned in correct order"),
		ok
	end)(),
	
	%% LIMIT && ORDERBY
	(fun() ->
		Docs = emongo:find(test1, "sushi", [], [{limit, 1}, {orderby, [{"rolls", asc}]}, {fields, [<<"rolls">>]}]),
		etap:is(length(Docs), 1, "limit returns one document"),
		etap:is(length(hd(Docs)), 2, "correct number of fields"),
		etap:is(lists:nth(2, hd(Docs)), {<<"rolls">>, 1}, "returned correct document from limit query"),
		ok
	end)(),
	
	%% LIMIT && OFFSET && ORDERBY
	(fun() ->
		Docs = emongo:find(test1, "sushi", [], [{limit, 1}, {offset, 1}, {orderby, [{"rolls", desc}]}]),
		etap:is(length(Docs), 1, "limit returns one document"),
		etap:is(lists:nth(2, hd(Docs)), {<<"rolls">>, 999}, "returned correct document from offset query"),
		ok
	end)(),
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{"rolls", 100}], [{orderby, [{"rolls", desc}]}]),
		etap:is(proplists:get_value(<<"rolls">>, hd(Docs)), 100, "query returned correct value"),
		ok
	end)(),
	
	%% NESTED QUERIES
	[emongo:insert(test1, "sushi", [{<<"seaweed">>, [{<<"sheets">>, I}]}]) || I <- lists:seq(1,10)],
	
	(fun() ->
		Docs = emongo:find(test1, "sushi", [{"seaweed.sheets", 5}]),
		etap:is(length(Docs), 1, "correct number of results from nested query"),
		etap:is(proplists:get_value(<<"seaweed">>, hd(Docs)), [{<<"sheets">>, 5}], "correct result returned"),
		ok
	end)(),
	

   	etap:end_tests().