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
-module(emongo_packet).

-export([insert/4, decode_response/1]).

-include("emongo.hrl").
	
insert(Database, Collection, ReqID, {obj, Props}) ->
	FullName = mongodb_bson:encode_cstring(lists:concat([Database, ".", Collection])),
	EncodedDocument = mongodb_bson:encode({obj, Props}),
	Message = <<0:32, FullName/binary, EncodedDocument/binary>>,
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_INSERT:32/little-signed, Message/binary>>.

decode_response(<<Length:32/little-signed, ReqID:32/little-signed, RespTo:32/little-signed, Op:32/little-signed, Message/binary>>) ->
	<<RespFlag:32/little-signed, 
	  CursorID:64/little-signed, 
	  StartingFrom:32/little-signed, 
	  NumRet:32/little-signed, 
	  Documents/binary>> = Message,
	#response{
		header = {header, Length, ReqID, RespTo, Op}, 
		responseFlag = RespFlag, 
		cursorID = CursorID, 
		startingFrom = StartingFrom, 
		numberReturned = NumRet, 
		documents = mongodb_bson:decode(Documents)
	}.