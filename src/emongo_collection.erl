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

fold(F, Acc0, PoolId, Collection, Selector) ->
	fold(F, Acc0, PoolId, Collection, Selector, []).

fold(F, Acc0, PoolId, Collection, Selector, Options) ->
    Documents = emongo:find_all(PoolId, Collection, Selector, Options),
    lists:foldl(F, Acc0, Documents).


%%%%%%%%%%%%%
%% foreach %%
%%%%%%%%%%%%%

foreach(F, PoolId, Collection) ->
	foreach(F, PoolId, Collection, [], []).

foreach(F, PoolId, Collection, Selector) ->
	foreach(F, PoolId, Collection, Selector, []).

foreach(F, PoolId, Collection, Selector, Options) ->
    Documents = emongo:find_all(PoolId, Collection, Selector, Options),
    lists:foreach(F, Documents).
