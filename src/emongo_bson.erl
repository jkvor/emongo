%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
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
-module(emongo_bson).
-export([encode/1, decode/1]).

encode(undefined) ->
	<<>>;
	
encode([]) ->
	<<>>;
	
encode([{_,_}|_]=List) when is_list(List) ->
	Bin = iolist_to_binary([encode_key_value(Key, Val) || {Key, Val} <- List]),
	<<(size(Bin)+5):32/little-signed, Bin/binary, 0:8>>.
		
%% FLOAT
encode_key_value(Key, Val) when is_float(Val) ->
	Key1 = encode_key(Key),
	<<1, Key1/binary, 0, Val:64/little-signed-float>>;

%% STRING
encode_key_value(Key, Val) when (is_list(Val) andalso length(Val) > 0 andalso is_integer(hd(Val))) orelse Val == [] ->
	Key1 = encode_key(Key),
	Val1 = unicode:characters_to_binary(Val),
	<<2, Key1/binary, 0, Val1/binary>>;
	
%% NESTED OBJECT
encode_key_value(Key, [{_,_}|_]=Val) ->
	Key1 = encode_key(Key),
	Val1 = encode(Val),
	<<3, Key1/binary, 0, Val1/binary>>;
	
%% DATA ARRAY
encode_key_value(Key, {array, Val}) when is_list(Val) ->
	Key1 = encode_key(Key),
	Val1 = encode(lists:zip(lists:seq(1, length(Val)), Val)),
	<<4, Key1/binary, 0, Val1/binary>>;
	
encode_key_value(Key, Val) when is_list(Val) ->
	encode_key_value(Key, {array, Val});

%% BINARY
encode_key_value(Key, Val) when is_binary(Val) ->	
	Key1 = encode_key(Key),
	<<5, Key1/binary, 0, Val/binary>>;
		
%% OID
encode_key_value(Key, {oid, Val}) ->
	Key1 = encode_key(Key),
	Val1 = iolist_to_binary(Val),
	<<7, Key1/binary, 0, Val1/binary>>;
	
%% BOOL
encode_key_value(Key, true) ->
	Key1 = encode_key(Key),
	<<8, Key1/binary, 0, 1:8>>;
	
encode_key_value(Key, false) ->
	Key1 = encode_key(Key),
	<<8, Key1/binary, 0, 0:8>>;	

%% DATE
encode_key_value(Key, {MegaSecs, Secs, MicroSecs}) when is_integer(MegaSecs), is_integer(Secs), is_integer(MicroSecs) ->
	Key1 = encode_key(Key),
	Secs1 = (MegaSecs * 1000000) + Secs,
	Epoch = Secs1 * 1000 + trunc(MicroSecs / 1000),
	<<9, Key1/binary, 0, Epoch:64/little-signed>>;

encode_key_value(Key, {datetime, Val}) ->
	Key1 = encode_key(Key),
	Date1 = calendar:datetime_to_gregorian_seconds(Val),
	Date2 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	Epoch = Date1 - Date2,
	<<9, Key1/binary, 0, Epoch:64/little-signed>>;
	
encode_key_value(Key, {{Year, Month, Day}, {Hour, Min, Secs}}) when is_integer(Year), is_integer(Month), is_integer(Day), is_integer(Hour), is_integer(Min), is_integer(Secs) ->
	encode_key_value(Key, {datetime, {{Year, Month, Day}, {Hour, Min, Secs}}});

%% VOID
encode_key_value(Key, undefined) ->
	Key1 = encode_key(Key),
	<<10, Key1/binary, 0>>;
	
% INT
encode_key_value(Key, Val) when is_integer(Val) ->
	Key1 = encode_key(Key),
	<<16, Key1/binary, 0, Val:32/little-signed>>;

encode_key_value(_, _) ->
	<<>>.
	
encode_key(Key) when is_atom(Key) ->
	atom_to_binary(Key, utf8);
	
encode_key(Key) when is_list(Key) ->
	unicode:characters_to_binary(Key);
	
encode_key(Key) when is_integer(Key) ->
	encode_key(integer_to_list(Key)).

decode(_) -> ok.

