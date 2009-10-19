#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell -config priv/example

main(_) ->
	etap:plan(unknown),
    error_logger:tty(false),
    etap_application:start_ok(emongo, "application 'emongo' started ok"),

	%% 1) data_number
	(fun() ->
		Val = 1.1,
		BinVal = <<Val:64/little-signed-float>>,
		Size = size(BinVal) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val}]), <<Size:32/little-unsigned, 1, 97, 0, BinVal/binary, 0>>, "data_number encodes ok"),
		ok
	end)(),
	
	%% 2) data_string
	(fun() ->
		Val = "abc",
		Val1 = unicode:characters_to_binary(Val),
		BinVal = <<(byte_size(Val1)+1):32/little-signed, Val1/binary, 0:8>>,
		Size = size(BinVal) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val}]), <<Size:32/little-unsigned, 2, 97, 0, BinVal/binary, 0>>, "data_string encodes ok"),
		ok
	end)(),
	
	%% 3) data_object
	(fun() ->
		Val = [{"b", "c"}],
		BinVal = emongo_bson:encode(Val),
		Size = size(BinVal) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val}]), <<Size:32/little-unsigned, 3, 97, 0, BinVal/binary, 0>>, "data_object encodes ok"),
		ok
	end)(),
	
	%% 4) data_array
	(fun() ->
		Val = {array, ["a", "b", "c"]},
		BinVal = emongo_bson:encode([{0, "a"}, {1, "b"}, {2, "c"}]),
		Size = size(BinVal) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val}]), <<Size:32/little-unsigned, 4, 97, 0, BinVal/binary, 0>>, "data_array encodes ok"),
		ok
	end)(),
	
	%% 5) data_binary
	(fun() ->
		Val = {binary, 2, <<"abc">>},
		BinVal = <<7:32/little-signed, 2:8, 3:32/little-signed, <<"abc">>/binary>>,
		Size = size(BinVal) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val}]), <<Size:32/little-unsigned, 5, 97, 0, BinVal/binary, 0>>, "data_binary encodes ok"),
		ok
	end)(),
	
	%% 7) data_oid
	(fun() ->
		Val1 = {oid, "ffffffffffffffffffffffff"},
		BinVal1 = emongo:hex2dec("ffffffffffffffffffffffff"),
		Size1 = size(BinVal1) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val1}]), <<Size1:32/little-unsigned, 7, 97, 0, BinVal1/binary, 0>>, "data_oid hex encodes ok"),
		
		Val2 = {oid, <<255,255,255,255,255,255,255,255,255,255,255,255>>},
		BinVal2 = <<255,255,255,255,255,255,255,255,255,255,255,255>>,
		Size2 = size(BinVal2) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val2}]), <<Size2:32/little-unsigned, 7, 97, 0, BinVal2/binary, 0>>, "data_oid dec encodes ok"),
		ok
	end)(),

	%% 8) data_boolean
	(fun() ->
		Val = true,
		BinVal = <<1:8>>,
		Size = size(BinVal) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val}]), <<Size:32/little-unsigned, 8, 97, 0, BinVal/binary, 0>>, "data_boolean encodes ok"),
		ok
	end)(),
	
	%% 9) data_date
	(fun() ->
		{MegaSecs,Secs,MicroSecs} = Val = now(),
		Secs1 = (MegaSecs * 1000000) + Secs,
		Epoch = Secs1 * 1000 + trunc(MicroSecs / 1000),
		BinVal = <<Epoch:64/little-signed>>,
		Size = size(BinVal) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val}]), <<Size:32/little-unsigned, 9, 97, 0, BinVal/binary, 0>>, "data_date encodes ok"),
		ok
	end)(),
	
	%% 10) data_null
	(fun() ->
		Val = undefined,
		BinVal = <<>>,
		Size = size(BinVal) + 8,
		etap:is(emongo_bson:encode([{<<"a">>, Val}]), <<Size:32/little-unsigned, 10, 97, 0, BinVal/binary, 0>>, "data_null encodes ok"),
		ok
	end)(),
	
   	etap:end_tests().