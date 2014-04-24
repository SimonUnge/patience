-module(patience_manager).
-behaviour(gen_server).

%% API
-export([start_link/0,
         show_piles/0,
         deal_cards/1,
         show_possible_moves/0,
         move_from_pile_empty_pile/1,
         pop_card_from_pile/1,
         show_pile_sizes/0
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

deal_cards(Cards) when length(Cards) =:= 4 ->
    gen_server:call(?MODULE, {deal_cards, Cards}).

show_piles() ->
    gen_server:call(?MODULE, {show_piles}).

show_possible_moves() ->
    gen_server:call(?MODULE, {show_possible_moves}).

move_from_pile_empty_pile(Pile) ->
    gen_server:call(?MODULE, {move_to_empty, Pile}).

pop_card_from_pile(Pile) ->
    gen_server:call(?MODULE, {pop_from_pile, Pile}).

show_pile_sizes() ->
    gen_server:call(?MODULE, {show_pile_sizes}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
    {ok, #state{}}.

handle_call({show_piles}, _From, State) ->
    {reply, State#state.piles, State};
handle_call({deal_cards, Cards}, _From, State) ->
    Piles = State#state.piles,
    NewPiles = pile_util:add_one_card_to_each_pile(Cards, Piles),
    {reply, NewPiles, State#state{piles = NewPiles}};
handle_call({show_possible_moves}, _From, State) ->
    Reply = {{piles_to_remove_from, pile_util:get_remove_from_piles(State#state.piles)},
             {empty_piles, pile_util:get_empty_piles(State#state.piles)}},
    {reply, Reply, State};
handle_call({move_to_empty, Pile}, _From, State) ->
    {Reply,NewState} = validate_and_act(fun is_valid_empty_move/2,
                                        fun pile_util:move_from_pile_to_empty_pile/2,
                                        Pile,
                                        State),
    {reply, Reply, NewState};
handle_call({pop_from_pile, Pile}, _From, State) ->
    {Reply,NewState} = validate_and_act(fun is_valid_pop/2,
                                        fun pile_util:pop_card_from_pile/2,
                                        Pile,
                                        State),
    {reply, Reply, NewState};
handle_call({show_pile_sizes}, _From, State) ->
    Reply = pile_util:get_pile_sizes(State#state.piles),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

is_valid_empty_move(Pile, Piles) ->
    PileCards = proplists:get_value(Pile, Piles),
    (1 < length(PileCards)) and pile_util:exists_empty_piles(Piles).

is_valid_pop(Pile, Piles) ->
    lists:member(Pile, pile_util:get_remove_from_piles(Piles)).

validate_and_act(Pred, Fun, Pile, State) ->
    case Pred(Pile ,State#state.piles) of
        true ->
            NewPiles = Fun(Pile, State#state.piles),
            Reply = ok,
            NewState = State#state{piles = NewPiles};
        false ->
            Reply = {error, "Invalid move."},
            NewState = State
    end,
    {Reply, NewState}.
