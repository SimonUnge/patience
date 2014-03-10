-module(pile_util).

-export([move_from_pile_to_pile/3,
         add_one_card_to_each_pile/2
        ]).

move_from_pile_to_pile(Piles, From, To) ->
    FromPile = proplists:get_value(From, Piles),
    ToPile = proplists:get_value(To, Piles),
    [FromCard | NewFromPile] = FromPile,
    NewToPile = [FromCard|ToPile],
    Piles1 = lists:keyreplace(From, 1, Piles, {From, NewFromPile}),
    lists:keyreplace(To, 1, Piles1, {To, NewToPile}).

add_one_card_to_each_pile([C1, C2, C3, C4], Piles) ->
    Pile1 = proplists:get_value(pile1, Piles),
    Pile2 = proplists:get_value(pile2, Piles),
    Pile3 = proplists:get_value(pile3, Piles),
    Pile4 = proplists:get_value(pile4, Piles),
    Piles1 = lists:keyreplace(pile1, 1, Piles, {pile1,[C1 | Pile1]}),
    Piles2 = lists:keyreplace(pile2, 1, Piles1, {pile2,[C2 | Pile2]}),
    Piles3 = lists:keyreplace(pile3, 1, Piles2, {pile3,[C3 | Pile3]}),
    lists:keyreplace(pile4, 1, Piles3, {pile4,[C4 | Pile4]}).
