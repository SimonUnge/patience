-module(pile_util).

-export([move_from_pile_to_empty_pile/3,
         add_one_card_to_each_pile/2,
         is_pile_empty/1,
         get_empty_piles/1
        ]).

move_from_pile_to_empty_pile(Piles, From, To) ->
    FromPile = proplists:get_value(From, Piles),
    ToPile = proplists:get_value(To, Piles),
    [FromCard | NewFromPile] = FromPile,
    NewToPile = [FromCard|ToPile],
    Piles1 = lists:keyreplace(From, 1, Piles, {From, NewFromPile}),
    lists:keyreplace(To, 1, Piles1, {To, NewToPile}).

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
