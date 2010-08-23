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
-compile(export_all).

encode([]) ->
	<<5,0,0,0,0>>;
	
encode([{_,_}|_]=List) when is_list(List) ->
	Bin = iolist_to_binary([encode_key_value(Key, Val) || {Key, Val} <- List]),
	<<(size(Bin)+5):32/little-signed, Bin/binary, 0:8>>.

encode_key_value(none, none) ->
    <<>>;
		
%% FLOAT
encode_key_value(Key, Val) when is_float(Val) ->
	Key1 = encode_key(Key),
	<<1, Key1/binary, 0, Val:64/little-signed-float>>;

%% STRING
%% binary string must be already in utf8
encode_key_value(Key, Val) when is_binary(Val) ->
	Key1 = encode_key(Key),
    <<2, Key1/binary, 0, (byte_size(Val)+1):32/little-signed, Val/binary, 0:8>>;

encode_key_value(Key, Val) when Val == [] orelse (is_list(Val) andalso length(Val) > 0 andalso is_integer(hd(Val))) ->
	Key1 = encode_key(Key),
	case unicode:characters_to_binary(Val) of
		{error, Bin, RestData} ->
			exit({cannot_convert_chars_to_binary, Val, Bin, RestData});
		{incomplete, Bin1, Bin2} ->
			exit({cannot_convert_chars_to_binary, Val, Bin1, Bin2});
		Val1 ->
			<<2, Key1/binary, 0, (byte_size(Val1)+1):32/little-signed, Val1/binary, 0:8>>
	end;
	
%% NESTED OBJECT
encode_key_value(Key, [{_,_}|_]=Val) ->
	Key1 = encode_key(Key),
	Val1 = encode(Val),
	<<3, Key1/binary, 0, Val1/binary>>;
	
%% DATA ARRAY
encode_key_value(Key, {array, Val}) when is_list(Val) ->
	Key1 = encode_key(Key),
	Val1 = encode(lists:zip(lists:seq(0, length(Val)-1), Val)),
	<<4, Key1/binary, 0, Val1/binary>>;
	
encode_key_value(Key, Val) when is_list(Val) ->
	encode_key_value(Key, {array, Val});

%% BINARY
encode_key_value(Key, {binary, 2, Val}) when is_binary(Val) ->
	Key1 = encode_key(Key),
	<<5, Key1/binary, 0, (byte_size(Val)+4):32/little-signed, 2:8, (size(Val)):32/little-signed, Val/binary>>;

encode_key_value(Key, {binary, SubType, Val}) when is_integer(SubType), is_binary(Val) ->
	Key1 = encode_key(Key),
	<<5, Key1/binary, 0, (byte_size(Val)):32/little-signed, SubType:8, Val/binary>>;
		
%% OID
encode_key_value(Key, {oid, HexString}) when is_list(HexString) ->
	encode_key_value(Key, {oid, emongo:hex2dec(HexString)});
	
encode_key_value(Key, {oid, OID}) when is_binary(OID) ->
	Key1 = encode_key(Key),
	<<7, Key1/binary, 0, OID/binary>>;
	
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
	Epoch = (Date1 - Date2) * 1000,
	<<9, Key1/binary, 0, Epoch:64/little-signed>>;
	
encode_key_value(Key, {{Year, Month, Day}, {Hour, Min, Secs}}) when is_integer(Year), is_integer(Month), is_integer(Day), is_integer(Hour), is_integer(Min), is_integer(Secs) ->
	encode_key_value(Key, {datetime, {{Year, Month, Day}, {Hour, Min, Secs}}});

%% VOID
encode_key_value(Key, undefined) ->
	Key1 = encode_key(Key),
	<<10, Key1/binary, 0>>;
	
%% REGEX
encode_key_value(Key, {regexp, Regexp, Options}) ->
	Key1 = encode_key(Key),
	RegexpBin = unicode:characters_to_binary(Regexp),
	OptionsBin = unicode:characters_to_binary(Options),
	<<11, Key1/binary, 0, RegexpBin/binary, 0, OptionsBin/binary, 0>>;
	
% INT
encode_key_value(Key, Val) when is_integer(Val), Val =< 2147483647, Val >= -2147483648 ->
	Key1 = encode_key(Key),
	<<16, Key1/binary, 0, Val:32/little-signed>>;

% LONG
encode_key_value(Key, Val) when is_integer(Val) ->
	Key1 = encode_key(Key),
	<<18, Key1/binary, 0, Val:64/little-signed>>;
	
encode_key_value(Key, Val) ->
	exit({oh_balls, Key, Val}).
	
encode_key(Key) when is_binary(Key) ->
	Key;
	
encode_key(Key) when is_atom(Key) ->
	atom_to_binary(Key, utf8);
	
encode_key(Key) when is_list(Key) ->
	unicode:characters_to_binary(Key);
	
encode_key(Key) when is_integer(Key) ->
	encode_key(integer_to_list(Key)).

decode(Bin) ->
	decode(Bin, []).
	
decode(<<>>, Acc) ->
	lists:reverse(Acc);
	
decode(Bin, Acc) ->
	{Doc, Rest} = decode_next(Bin),
	decode(Rest, [Doc|Acc]).
	
decode_next(<<Size:32/little-signed, Rest/binary>>) ->
	Size1 = Size-5,
	<<Bin:Size1/binary, 0:8, Tail/binary>> = Rest,
	{decode_document(Bin, []), Tail}.
	
decode_document(<<>>, Acc) ->
	lists:reverse(Acc);
	
decode_document(<<Type:8/little-signed, Tail1/binary>>, Acc) ->
	{Key, Tail2} = decode_key(Tail1, <<>>),
	{Val, Tail3} = decode_value(Type, Tail2),
	decode_document(Tail3, [{Key, Val}|Acc]).

decode_key(<<0, Tail/binary>>, Acc) ->
	{Acc, Tail};
	
decode_key(<<H:8, Tail/binary>>, Acc) ->
	decode_key(Tail, <<Acc/binary, H>>).
	
%% DOUBLE
decode_value(1, <<Val:64/little-signed-float, Tail/binary>>) ->
	{Val, Tail};
	
%% STRING
decode_value(2, <<Size:32/little-signed, Tail1/binary>>) ->
	Size1 = Size-1,
	<<Val:Size1/binary, 0, Tail2/binary>> = Tail1,
	{Val, Tail2};
	
%% OBJECT
decode_value(3, Bin) ->
	decode_next(Bin);

%% DATA ARRAY
decode_value(4, Bin) ->
	{Val, Rest} = decode_next(Bin),
	{{array, [V || {_, V} <- Val]}, Rest};

%% BINARY
decode_value(5, <<_Size:32/little-signed, 2:8/little, BinSize:32/little-signed, BinData:BinSize/binary-little-unit:8, Tail/binary>>) ->
  	{{binary, 2, BinData}, Tail};

decode_value(5, <<Size:32/little-signed, SubType:8/little, BinData:Size/binary-little-unit:8, Tail/binary>>) ->
  	{{binary, SubType, BinData}, Tail};

%% OID
decode_value(7, <<OID:12/binary, Tail/binary>>) ->
	{{oid, OID}, Tail};
	
%% BOOL	
decode_value(8, <<0:8, Tail/binary>>) ->
	{false, Tail};
	
decode_value(8, <<1:8, Tail/binary>>) ->
	{true, Tail};

%% DATE
decode_value(9, <<MSecs:64/little-signed, Tail/binary>>) ->
	UnixTime = trunc(MSecs / 1000),
	MegaSecs = trunc(UnixTime / 1000000),
	Secs = UnixTime - (MegaSecs * 1000000),
	MicroSecs = (MSecs - (UnixTime * 1000)) * 1000,
	{{MegaSecs, Secs, MicroSecs}, Tail};

%% VOID
decode_value(10, Tail) ->
	{undefined, Tail};

%% INT
decode_value(16, <<Int:32/little-signed, Tail/binary>>) ->
	{Int, Tail};

%% Timestamp
decode_value(17, <<Inc:32/little-signed, Timestamp:32/little-signed, Tail/binary>>) ->
    {{timestamp, Inc, Timestamp}, Tail};
		
%% LONG
decode_value(18, <<Int:64/little-signed, Tail/binary>>) ->
	{Int, Tail};
	
decode_value(Type, Tail) ->
	exit({emongo_unknown_type, Type, Tail}).
