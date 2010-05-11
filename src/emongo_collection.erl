%% @doc Collection utility functions
%% @author Jacob Perkins <japerk@gmail.com>
-module(emongo_collection).

-include("emongo.hrl").

-export([distinct_set/4, distinct_list/4]).
-export([fold/4, fold/5, fold/6]).
-export([foreach/3, foreach/4, foreach/5]).

%%%%%%%%%%%%%%
%% distinct %%
%%%%%%%%%%%%%%

distinct_set(PoolId, Collection, Key, Selector) ->
	F = fun({array, List}, Set) -> sets:union(Set, sets:from_list(List)) end,
	Arrays = emongo:distinct(PoolId, Collection, Key, Selector),
	lists:foldl(F, sets:new(), Arrays).

distinct_list(PoolId, Collection, Key, Selector) ->
	lists:sort(sets:to_list(distinct_set(PoolId, Collection, Key, Selector))).

%%%%%%%%%%
%% fold %%
%%%%%%%%%%

fold(F, Acc0, PoolId, Collection) ->
	fold(F, Acc0, PoolId, Collection, [], [{timeout, ?TIMEOUT}]).

fold(F, Acc0, PoolId, Collection, Selector) when ?IS_DOCUMENT(Selector) ->
	fold(F, Acc0, PoolId, Collection, Selector, [{timeout, ?TIMEOUT}]).

fold(F, Acc0, PoolId, Collection, Selector, Options) when ?IS_DOCUMENT(Selector), is_list(Options) ->
	Opts = [response_options|Options],
	Resp = emongo:find(PoolId, Collection, Selector, Opts),
	Acc1 = lists:foldl(F, Acc0, Resp#response.documents),
	fold(F, Acc1, PoolId, Collection, Selector, Opts, Resp).

fold(_, Acc, _, _, _, _, Resp) when is_record(Resp, response), Resp#response.cursor_id == 0 ->
	Acc;
fold(F, Acc0, PoolId, Collection, Selector, Options, Resp) when is_record(Resp, response) ->
	Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
	Resp1 = emongo:get_more(PoolId, Collection, Resp#response.cursor_id, Timeout),
	Acc1 = lists:foldl(F, Acc0, Resp1#response.documents),
	fold(F, Acc1, PoolId, Collection, Selector, Options, Resp1).

%%%%%%%%%%%%%
%% foreach %%
%%%%%%%%%%%%%

foreach(F, PoolId, Collection) ->
	foreach(F, PoolId, Collection, [], [{timeout, ?TIMEOUT}]).

foreach(F, PoolId, Collection, Selector) when ?IS_DOCUMENT(Selector) ->
	foreach(F, PoolId, Collection, Selector, [{timeout, ?TIMEOUT}]).

foreach(F, PoolId, Collection, Selector, Options) when ?IS_DOCUMENT(Selector), is_list(Options) ->
	Opts = [response_options|Options],
	Resp = emongo:find(PoolId, Collection, Selector, Opts),
	lists:foreach(F, Resp#response.documents),
	foreach(F, PoolId, Collection, Selector, Opts, Resp).

foreach(_, _, _, _, _, Resp) when is_record(Resp, response), Resp#response.cursor_id == 0 ->
	ok;
foreach(F, PoolId, Collection, Selector, Options, Resp) when is_record(Resp, response) ->
	Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
	Resp1 = emongo:get_more(PoolId, Collection, Resp#response.cursor_id, Timeout),
	lists:foreach(F, Resp1#response.documents),
	foreach(F, PoolId, Collection, Selector, Options, Resp1).