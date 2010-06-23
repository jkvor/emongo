-module(pqueue_test).

-compile(export_all).

test(Data, F) ->
    {Time1, Sorted1} = timer:tc(lists, sort, [Data]),
    {Time21, Pushed1} = timer:tc(lists, foldl, [fun({P, I}, Q) ->
                                                        pqueue:push(P, I, Q)
                                                end, pqueue:new(), Data]),
    
    Sorted = [I || {_, V} = I <- Sorted1, F(V)],
    Pushed = pqueue:filter(F, Pushed1),
    
    {Time22, _} = timer:tc(?MODULE, pop_items, [Pushed]),
    Time2 = Time21 + Time22,
    
    io:format("Time sorted: ~p~nTime pushed: ~p~nDivide: ~p~n", [Time1, Time2, Time1/Time2]),
    
    LenS = length(Sorted),
    LenP = pqueue:size(Pushed),
    
    LenS = LenP,
    io:format("Length: ~p~n", [LenS]),

    lists:foldl(fun({_, I}, Q) ->
                        {I, NQ} = pqueue:pop(Q),
                        NQ
                end, Pushed, Sorted).

test() ->
    Len = 10000,
    Dev = 10,
    Fun = fun(I) -> I rem 3 =/= 0 end,
    
    Data = [{random:uniform(Dev), I} || I <- lists:seq(1, Len)],
    test(Data, Fun).

test2() ->
    Queue0 = push_items([{1, 1}, {1, 2}, {5, 3}, {2, 4}], pqueue:new()),
    4 = pqueue:size(Queue0),
    
    Queue1 = ensure_items([1, 2, 4], Queue0),
    1 = pqueue:size(Queue1),
    
    Queue2 = push_items([{2, 5}, {3, 6}], Queue1),
    3 = pqueue:size(Queue2),
    
    Queue3 = ensure_items([5, 3, 6], Queue2),
    
    Queue3 = pqueue:new().
    
push_items(Data, Queue) ->
    lists:foldl(fun({P, I}, Q) -> pqueue:push(P, I, Q) end,
                Queue,
                Data).

ensure_items(Items, Queue) ->
    lists:foldl(fun(I, Q) -> {I, NQ} = pqueue:pop(Q), NQ end,
                Queue,
                Items).

pop_items(Queue) ->
    case pqueue:size(Queue) of
        0 ->
            ok;
        _ ->
            {_Item, NewQueue} = pqueue:pop(Queue),
            pop_items(NewQueue)
    end.
