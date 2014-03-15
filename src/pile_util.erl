-module(pile_util).

-export([move_from_pile_to_empty_pile/2,
         add_one_card_to_each_pile/2,
         is_pile_empty/1,
         get_empty_piles/1
        ]).

move_from_pile_to_empty_pile(Piles, From) ->

    case get_empty_piles(Piles) of
        [] ->
            {error, "No empty piles"};
        EmptyPiles ->
            To = hd(EmptyPiles),
            FromPile = proplists:get_value(From, Piles),
            [FromCard | NewFromPile] = FromPile,
            Piles1 = lists:keyreplace(From, 1, Piles, {From, NewFromPile}),
            lists:keyreplace(To, 1, Piles1, {To, [FromCard]})
    end.

add_one_card_to_each_pile(Cards, Piles) ->
    lists:map(fun add_card_to_pile/1,
              lists:zip(Cards,Piles)).

add_card_to_pile({Card,{PileN,Pile}}) ->
    {PileN, [Card | Pile]}.

is_pile_empty({_,[]}) ->
    true;
is_pile_empty({_,_Cards}) ->
    false.

get_empty_piles(Piles) ->
    [element(1, X) || X <- Piles, is_pile_empty(X)].
