-module(pile_util).

-export([move_from_pile_to_pile/3]).

move_from_pile_to_pile(Piles, From, To) ->
    FromPile = proplists:get_value(From, Piles),
    ToPile = proplists:get_value(To, Piles),
    [FromCard | NewFromPile] = FromPile,
    NewToPile = [FromCard|ToPile],
    Piles1 = lists:keyreplace(From, 1, Piles, {From, NewFromPile}),
    lists:keyreplace(To, 1, Piles1, {To, NewToPile}).
