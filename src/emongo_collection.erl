%% @doc Collection utility functions
%% @author Jacob Perkins <japerk@gmail.com>
-module(emongo_collection).

-include("emongo.hrl").

-export([fold/4, fold/5, fold/6]).
-export([foreach/3, foreach/4, foreach/5]).

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
