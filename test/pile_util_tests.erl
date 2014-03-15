-module(pile_util_tests).

-include_lib("eunit/include/eunit.hrl").

pile_is_empty_test() ->
    ?assert(pile_util:is_pile_empty({pile2,[]})).

pile_is_not_empty_test() ->
    ?assertNot(pile_util:is_pile_empty({pile1,[1,2,3]})).

fail_move_from_empty_pile_to_empty_pile_test() ->
    Piles = [{pile1,[]},{pile2,[]}],
    ?assertError({badmatch, []}, pile_util:move_from_pile_to_empty_pile(Piles, pile1)).

move_from_pile_to_empty_pile_test() ->
    Piles = [{pile1,[1,2,3]},{pile2,[]}],
    ?assertEqual([{pile1,[2,3]},{pile2, [1]}],pile_util:move_from_pile_to_empty_pile(Piles, pile1)).

move_from_pile_to_empty_pile_no_empty_piles_test() ->
    Piles = [{pile1,[1,2,3]},{pile2,[1,2]}],
    ?assertError(badarg,pile_util:move_from_pile_to_empty_pile(Piles, pile1)).

add_one_card_to_each_pile_test() ->
    Piles = [{pile1,[]},{pile2,[]},{pile3,[]},{pile4,[]}],
    FourCards = [{hearts,14},{diamonds,3},{diamonds,13},{clubs,5}],
    ?assertEqual([{pile1,[{hearts,14}]},{pile2,[{diamonds,3}]},{pile3,[{diamonds,13}]},{pile4,[{clubs,5}]}],
                 pile_util:add_one_card_to_each_pile(FourCards, Piles)).

return_two_empty_piles_test() ->
    Piles = [{pile1,[1,2]},{pile2,[]},{pile3,[1,2]},{pile4,[]}],
    ?assertEqual([pile2,pile4], pile_util:get_empty_piles(Piles)).

no_empty_piles_test() ->
    Piles = [{pile1,[1,2]},{pile2,[1,2]},{pile3,[1,2]},{pile4,[2,3]}],
    ?assert(pile_util:exists_empty_piles(Piles)).
