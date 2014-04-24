-module(pile_util).

-export([move_from_pile_to_empty_pile/2,
         add_one_card_to_each_pile/2,
         is_pile_empty/1,
         get_empty_piles/1,
         exists_empty_piles/1,
         get_remove_from_piles/1,
         pop_card_from_pile/2,
         get_pile_sizes/1
        ]).

move_from_pile_to_empty_pile(From, Piles) ->
    EmptyPiles = get_empty_piles(Piles),
    To = hd(EmptyPiles),
    FromPile = proplists:get_value(From, Piles),
    [FromCard | NewFromPile] = FromPile,
    Piles1 = lists:keyreplace(From, 1, Piles, {From, NewFromPile}),
    lists:keyreplace(To, 1, Piles1, {To, [FromCard]}).

add_one_card_to_each_pile(Cards, Piles) ->
    lists:zipwith(fun add_card_to_pile/2, Cards,Piles).

add_card_to_pile(Card,{PileN,Pile}) ->
    {PileN, [Card | Pile]}.

get_empty_piles(Piles) ->
    [element(1, X) || X <- Piles, is_pile_empty(X)].

is_pile_empty({_,[]}) ->
    true;
is_pile_empty({_,_Cards}) ->
    false.

exists_empty_piles(Piles) ->
    [] =/= get_empty_piles(Piles).

get_remove_from_piles(Piles) ->
    [element(1,X) || X <- Piles, ok_to_remove(X, Piles)].

ok_to_remove({_,[]}, _) ->
    false;
ok_to_remove({Pile,_}, Piles) ->
    Card = hd(proplists:get_value(Pile, Piles)),
    any(fun any_higer_card/2, Card, Piles).

any(Pred, Card, [H|T]) ->
    case Pred(Card,H) of
        true -> true;
        false -> any(Pred, Card, T)
    end;
any(_Pred, _Card, []) ->
    false.

any_higer_card({Suite, Value}, {_, [{Suite,Value2}|_]}) when Value < Value2 ->
    true;
any_higer_card(_,_) ->
    false.

pop_card_from_pile(Pile, Piles) ->
    PileCards = proplists:get_value(Pile, Piles),
    [_ | NewPileCards] = PileCards,
    lists:keyreplace(Pile, 1, Piles, {Pile, NewPileCards}).

get_pile_sizes(Piles) ->
    [{P, length(C)} || {P, C} <- Piles].
