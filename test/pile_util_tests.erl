-module(pile_util_tests).

-include_lib("eunit/include/eunit.hrl").

move_from_pile_to_pile_test() ->
    L = [{pile1,[1,2,3]},{pile2,[4,5]}],
    ?assertEqual([{pile1,[2,3]},{pile2, [1,4,5]}],pile_util:move_from_pile_to_pile(L, pile1, pile2)).
