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

-export([update/6, insert/4, do_query/4, get_more/5, 
		 delete/4, kill_cursors/2, msg/2, decode_response/1,
		 ensure_index/4, get_last_error/2, server_status/2]).

-include("emongo.hrl").

get_last_error(Database, ReqId) ->
    %%Query = #emo_query{q=[{<<"getlasterror">>, 1}], limit=1},
    %%do_query(Database, "$cmd", ReqId, Query).
    DatabaseLength = byte_size(Database),
    <<(57+DatabaseLength):32/little-signed, ReqId:32/little-signed, 0:32,
     ?OP_QUERY:32/little-signed, 0:32, Database/binary, ".$cmd", 0, 0:32, 1:32/little-signed,
     %% Encoded document
     23:32/little-signed, 16, "getlasterror", 0, 1:32/little-signed, 0>>.

server_status(Database, ReqId) ->
    %%Query = #emo_query{q=[{<<"serverStatus">>, 1}], limit=1},
    %%do_query(Database, "$cmd", ReqId, Query).
    DatabaseLength = byte_size(Database),
    <<(57+DatabaseLength):32/little-signed, ReqId:32/little-signed, 0:32,
     ?OP_QUERY:32/little-signed, 0:32, Database/binary, ".$cmd", 0, 0:32, 1:32/little-signed,
     %% Encoded document
     23:32/little-signed, 16, "serverStatus", 0, 1:32/little-signed, 0>>.

update(Database, Collection, ReqID, Upsert, Selector, Document) ->
	FullName = unicode:characters_to_binary([Database, ".", Collection]),
	EncodedSelector = emongo_bson:encode(Selector),
	EncodedDocument = emongo_bson:encode(Document),
	BinUpsert = if Upsert == true -> 1; true -> 0 end,
	Message = <<0:32, FullName/binary, 0, BinUpsert:32/little-signed, EncodedSelector/binary, EncodedDocument/binary>>,
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_UPDATE:32/little-signed, Message/binary>>.

insert(Database, Collection, ReqID, Documents) ->
	FullName = unicode:characters_to_binary([Database, ".", Collection]),
	EncodedDocument = iolist_to_binary([emongo_bson:encode(Document) || Document <- Documents]),
	Message = <<0:32, FullName/binary, 0, EncodedDocument/binary>>,
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_INSERT:32/little-signed, Message/binary>>.

do_query(Database, Collection, ReqID, Query) when is_record(Query, emo_query) ->
	OptsSum = lists:foldl(fun(X, Acc) -> Acc+X end, 0, Query#emo_query.opts),
	FullName = unicode:characters_to_binary([Database, ".", Collection]),
	EncodedDocument = if 
		is_binary(Query#emo_query.q) -> Query#emo_query.q; 
		true -> emongo_bson:encode(Query#emo_query.q)
	end,
	EncodedFieldSelector = if 
		Query#emo_query.field_selector == [] -> <<>>; 
		true -> emongo_bson:encode(Query#emo_query.field_selector) 
	end,
	Message = <<OptsSum:32/little-signed, FullName/binary, 0:8,
				(Query#emo_query.offset):32/little-signed, 
				(Query#emo_query.limit):32/little-signed, 
				EncodedDocument/binary, EncodedFieldSelector/binary>>,
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_QUERY:32/little-signed, Message/binary>>.

get_more(Database, Collection, ReqID, NumToReturn, CursorID) ->
	FullName = unicode:characters_to_binary([Database, ".", Collection]),
	Message = <<0:32, FullName/binary, 0, NumToReturn:32/little-signed, CursorID:64/little-signed>>,
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_GET_MORE:32/little-signed, Message/binary>>.
	
delete(Database, Collection, ReqID, Selector) ->
	FullName = unicode:characters_to_binary([Database, ".", Collection]),
	EncodedDocument = emongo_bson:encode(Selector),
	Message = <<0:32, FullName/binary, 0, 0:32, EncodedDocument/binary>>,
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_DELETE:32/little-signed, Message/binary>>.

ensure_index(Database, Collection, ReqID, Keys) ->
	FullName = unicode:characters_to_binary([Database, ".system.indexes"]),
	Selector = [
		{<<"name">>, index_name(Keys, <<>>)},
		{<<"ns">>, unicode:characters_to_binary([Database, ".", Collection])},
		{<<"key">>, Keys}],
	EncodedDocument = emongo_bson:encode(Selector),
	Message = <<0:32, FullName/binary, 0, EncodedDocument/binary>>,
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_INSERT:32/little-signed, Message/binary>>.

kill_cursors(ReqID, CursorIDs) ->
	CursorIDsBin = lists:foldl(fun(ID, Bin) -> <<Bin/binary, ID:64/little-signed>> end, <<>>, CursorIDs),
	Message = <<0:32, (length(CursorIDs)):32/little-signed, CursorIDsBin/binary>>,
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_KILL_CURSORS:32/little-signed, Message/binary>>.

msg(ReqID, Message) ->
	Length = byte_size(Message),
    <<(Length+16):32/little-signed, ReqID:32/little-signed, 0:32, ?OP_MSG:32/little-signed, Message/binary>>.
	
decode_response(<<Length:32/little-signed, ReqID:32/little-signed, RespTo:32/little-signed, Op:32/little-signed, Message/binary>>) ->
	MsgLen = Length - 16,
	if 
		byte_size(Message) < MsgLen ->
			undefined;
		true ->
			DocLen = MsgLen - 20,
			<<RespFlag:32/little-signed, 
			  CursorID:64/little-signed, 
			  StartingFrom:32/little-signed, 
			  NumRet:32/little-signed, 
			  Documents:DocLen/binary,
			  Tail/binary>> = Message,
			Resp = #response{
				header = #header{message_length = Length,
                                                 request_id = ReqID,
                                                 response_to = RespTo,
                                                 op_code = Op},
				response_flag = RespFlag, 
				cursor_id = CursorID, 
				offset = StartingFrom, 
				limit = NumRet, 
				documents = Documents
			},
			{Resp, Tail}
	end;

decode_response(_) ->
    undefined.

index_name([], Bin) -> Bin;
index_name([{Key, Val}|Tail], Bin) ->
	KeyBin = unicode:characters_to_binary(Key),
	ValBin = if
		is_integer(Val) -> list_to_binary(integer_to_list(Val));
		true -> <<>>
	end,
	index_name(Tail, <<Bin/binary, KeyBin/binary, "_", ValBin/binary>>).
