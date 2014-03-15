-module(pm_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([
         get_four_empty_piles/1
        ]).

-define(EMPTY_PILES,[{pile1, []}, {pile2, []}, {pile3, []}, {pile4, []}]).

all() ->
    [
     {group, pm_utils}
    ].

groups() ->
    [
     {pm_utils,
      [],
      [
       get_four_empty_piles
      ]
     }
    ].

get_four_empty_piles(_Config) ->
    ?EMPTY_PILES = patience_manager:show_piles().

init_per_group(pm_utils, Config) ->
    start_app_return_config(Config);
init_per_group(_, Config) ->
    Config.

end_per_group(pm_utils, _Config) ->
    stop_app();
end_per_group(_, _Config) ->
    ok.

start_app_return_config(Config) ->
    ok = application:start(deckerl),
    ok = application:start(patience_game),
    Config.

stop_app() ->
    ok = application:stop(patience_game).
