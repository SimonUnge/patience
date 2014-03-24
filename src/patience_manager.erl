-module(patience_manager).
-behaviour(gen_server).

%% API
-export([start_link/0,
         show_piles/0,
         draw_cards/0,
         show_possible_moves/0,
         move_from_pile_empty_pile/1,
         pop_card_from_pile/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {piles = [{pile1, []},
                         {pile2, []},
                         {pile3, []},
                         {pile4, []}]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

draw_cards() ->
    gen_server:call(?MODULE, {draw_cards}).

show_piles() ->
    gen_server:call(?MODULE, {show_piles}).

show_possible_moves() ->
    gen_server:call(?MODULE, {show_possible_moves}).

move_from_pile_empty_pile(Pile) ->
    gen_server:call(?MODULE, {move_to_empty, Pile}).

pop_card_from_pile(Pile) ->
    gen_server:call(?MODULE, {pop_from_pile, Pile}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
    deck_manager:shuffle_deck(),
    {ok, #state{}}.

handle_call({show_piles}, _From, State) ->
    {reply, State#state.piles, State};
handle_call({draw_cards}, _From, State) ->
    case deck_manager:deck_size() of
        N when N > 3 ->
            Cards = draw_four_cards(),
            Piles = State#state.piles,
            NewPiles = pile_util:add_one_card_to_each_pile(Cards, Piles),
            {reply, NewPiles, State#state{piles = NewPiles}};
        N when N =< 3 ->
            {reply, "No more cards left in the deck.", State}
    end;
handle_call({show_possible_moves}, _From, State) ->
    Reply = {{piles_to_remove_from, pile_util:get_remove_from_piles(State#state.piles)},
             {empty_piles, pile_util:get_empty_piles(State#state.piles)}},
    {reply, Reply, State};
handle_call({move_to_empty, Pile}, _From, State) ->
    case is_non_empty_and_empty_exists(Pile ,State#state.piles) of
        true ->
            NewPiles = pile_util:move_from_pile_to_empty_pile(State#state.piles, Pile),
            Reply = ok,
            NewState = State#state{piles = NewPiles};
        false ->
            Reply = {error, "Invalid move."},
            NewState = State
    end,
    {reply, Reply, NewState};
handle_call({pop_from_pile, Pile}, _From, State) ->
    case is_valid_pop(Pile, State#state.piles) of
        true ->
            NewState = State#state{piles = pile_util:pop_card_from_pile(Pile, State#state.piles)},
            Reply = ok;
        false ->
            Reply = {error, "Invalid move."},
            NewState = State
    end,
    {reply, Reply, NewState}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

draw_four_cards() ->
    deck_manager:draw_N_cards(4).

is_non_empty_and_empty_exists(Pile, Piles) ->
    ([] =/= proplists:get_value(Pile, Piles)) and pile_util:exists_empty_piles(Piles).

is_valid_pop(Pile, Piles) ->
    lists:member(Pile, pile_util:get_remove_from_piles(Piles)).
