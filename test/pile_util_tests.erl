-module(pile_util_tests).

-include_lib("eunit/include/eunit.hrl").

pile_is_empty_test() ->
    ?assert(pile_util:is_pile_empty({pile2,[]})).

pile_is_not_empty_test() ->
    ?assertNot(pile_util:is_pile_empty({pile1,[1,2,3]})).

fail_move_from_empty_pile_to_empty_pile_test() ->
    Piles = [{pile1,[]},{pile2,[]}],
    ?assertError({badmatch, []}, pile_util:move_from_pile_to_empty_pile(pile1, Piles)).

move_from_pile_to_empty_pile_test() ->
    Piles = [{pile1,[1,2,3]},{pile2,[]}],
    ?assertEqual([{pile1,[2,3]},{pile2,
    [1]}],pile_util:move_from_pile_to_empty_pile(pile1, Piles)).

move_from_pile_to_empty_pile_no_empty_piles_test() ->
    Piles = [{pile1,[1,2,3]},{pile2,[1,2]}],
    ?assertError(badarg,pile_util:move_from_pile_to_empty_pile(pile1, Piles)).

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
    ?assertNot(pile_util:exists_empty_piles(Piles)).

possible_to_remove_card_from_pile_3_test() ->
    Piles = [{pile1, [{heart,10}]},
             {pile2, [{spade, 3}]},
             {pile3, [{heart,4},{heart,5}]},
             {pile4, [{club, 2}]}],
    PilesToRemoveFrom = [pile3],
    ?assertEqual(PilesToRemoveFrom, pile_util:get_remove_from_piles(Piles)).

possible_to_remove_card_from_pile_2_test() ->
    Piles = [{pile1, [{heart,10}]},
             {pile2, [{heart, 3}]},
             {pile3, [{spade,4},{heart,5}]},
             {pile4, []}],
    PilesToRemoveFrom = [pile2],
    ?assertEqual(PilesToRemoveFrom, pile_util:get_remove_from_piles(Piles)).

possible_to_remove_card_from_pile_2_and_1_test() ->
    Piles = [{pile1, [{heart,10}, {club, 2}]},
             {pile2, [{heart, 3}]},
             {pile3, [{heart, 11},{heart,5}]},
             {pile4, []}],
    PilesToRemoveFrom = [pile1, pile2],
    ?assertEqual(PilesToRemoveFrom, pile_util:get_remove_from_piles(Piles)).

pop_card_from_pile_test() ->
    Piles = [{pile1, [{heart,10}, {club, 2}]},
             {pile2, [{heart, 3}]},
             {pile3, [{heart, 11},{heart,5}]},
             {pile4, []}],
    PoppedPiles = [{pile1, [{club, 2}]},
                  {pile2, [{heart, 3}]},
                  {pile3, [{heart, 11},{heart,5}]},
                  {pile4, []}],
    ?assertEqual(PoppedPiles, pile_util:pop_card_from_pile(pile1, Piles)).

pop_card_from_empty_pile_test() ->
    Piles = [{pile1, [{heart,10}, {club, 2}]},
             {pile2, [{heart, 3}]},
             {pile3, [{heart, 11},{heart,5}]},
             {pile4, []}],
    ?assertError({badmatch,[]}, pile_util:pop_card_from_pile(pile4, Piles)).
