-module(pile_util_tests).

-include_lib("eunit/include/eunit.hrl").

move_from_pile_to_pile_test() ->
    L = [{pile1,[1,2,3]},{pile2,[4,5]}],
    ?assertEqual([{pile1,[2,3]},{pile2, [1,4,5]}],pile_util:move_from_pile_to_pile(L, pile1, pile2)).

add_one_card_to_each_pile_test() ->
    Piles = [{pile1,[]},{pile2,[]},{pile3,[]},{pile4,[]}],
    FourCards = [{hearts,14},{diamonds,3},{diamonds,13},{clubs,5}],
    ?assertEqual([{pile1,[{hearts,14}]},{pile2,[{diamonds,3}]},{pile3,[{diamonds,13}]},{pile4,[{clubs,5}]}],
                pile_util:add_one_card_to_each_pile(FourCards, Piles)).
