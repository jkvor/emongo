%% Copyright (c) 2010 Belyaev Dmitry <rumata-estor@nm.ru>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% Is pairing heap a better realization?
-module(pqueue).

-export([
         new/0,
         size/1,
         push/3,
         pop/1,
         filter/2
        ]).

-type tail() :: {integer(), [term()]}.

-record(pqueue, {size :: integer(),
                 head :: [term()],
                 tails :: [tail()]}).

-spec new() -> #pqueue{}.
new() ->
    #pqueue{size = 0, head = [], tails = []}.


-spec size(#pqueue{}) -> integer().
size(#pqueue{size = Size}) ->
    Size.


-spec push(integer(), term(), #pqueue{}) -> #pqueue{}.
push(Priority, Item, #pqueue{size = Size, head = Head, tails = Tails}) ->
    #pqueue{size = Size + 1, head = Head, tails = push_item(Tails, Priority, Item)}.


-spec pop(#pqueue{}) -> {term(), #pqueue{}} | empty.
pop(#pqueue{size = Size, head = [Item | Head], tails = Tails}) ->
    {Item, #pqueue{size = Size - 1, head = Head, tails = Tails}};

pop(#pqueue{size = Size, head = [], tails = [{_, Tail} | Tails]}) ->
    [Item | Head] = lists:reverse(Tail),
    {Item, #pqueue{size = Size - 1, head = Head, tails = Tails}};

pop(_) ->
    empty.


-spec filter(fun((term()) -> boolean()), #pqueue{}) -> #pqueue{}.
filter(_, #pqueue{size = 0} = Q) ->
    Q;
filter(Fun, #pqueue{head = Head, tails = Tails}) ->
    NewHead = [I || I <- Head, Fun(I)],
    NewTails = filter_tails(Fun, Tails),
    
    NewSize = length(Head) + lists:sum([length(Items) || {_, Items} <- NewTails]),
    #pqueue{size = NewSize, head = NewHead, tails = NewTails}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec push_item([tail()], integer(), term()) -> [tail()].
push_item([], Priority, Item) ->
    [{Priority, [Item]}];

push_item([{Priority, Items} | Tails], Priority, Item) ->
    [{Priority, [Item | Items]} | Tails];

push_item([{LowerP, _} = LowerI | Tails], Priority, Item)
  when LowerP < Priority ->
    [LowerI | push_item(Tails, Priority - LowerP, Item)];

push_item([{HigherP, Items} | Tails], Priority, Item) ->
    [{Priority, [Item]}, {HigherP - Priority, Items} | Tails].


-spec filter_tails(fun((term()) -> boolean()), [tail()]) -> [tail()].
filter_tails(_, []) ->
    [];
filter_tails(Fun, [{Priority, Items} | Tails]) ->
    case [I || I <- Items, Fun(I)] of
        [] ->
            filter_tails(Fun, add_priority(Priority, Tails));
        NewItems ->
            [{Priority, NewItems} | filter_tails(Fun, Tails)]
    end.


-spec add_priority(integer(), [tail()]) -> [tail()].
add_priority(_, []) ->
    [];
add_priority(Priority, [{NextP, Items} | Tails]) ->
    [{NextP + Priority, Items} | Tails].
