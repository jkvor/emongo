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
	
%% encode list of key/values
encode([{_, _}|_]=Items) ->
	iolist_to_binary([encode(Item) || Item <- Items]);
	
encode({Key, Val}) when is_float(Val) ->
	Key1 = encode(Key),
	Val1 = encode_double(Val),
	<<1, Key1/binary, Val1/binary>>;

encode({Key, Val}) when (is_list(Val) andalso length(Val) > 0 andalso is_integer(hd(Val))) orelse Val == [] ->
	Key1 = encode(Key),
	Val1 = encode_string(Val),
	<<2, Key1/binary, Val1/binary>>;
	
encode({Key, [{_,_}|_]=Val}) ->
	Key1 = encode(Key),
	Val1 = encode(Val),
	<<3, Key1/binary, Val1/binary>>;

encode({Key, Val}) when is_binary(Val) ->	
	Key1 = encode(Key),
	<<5, Key1/binary, Val/binary>>;
		
encode(Val) when is_atom(Val) ->
	erlang:atom_to_binary(Val, utf8);
	
encode(Val) when is_binary(Val) ->
	Val;
		
encode(Val) when (is_list(Val) andalso length(Val) > 0 andalso is_integer(hd(Val))) orelse Val == [] -> 
	unicode:characters_to_binary(Val).
	
encode_string(Val) ->
	unicode:characters_to_binary(Val).
	
encode_double(Val) ->
	<<Val:64/little-signed-float>>.
	
decode(_) -> ok.