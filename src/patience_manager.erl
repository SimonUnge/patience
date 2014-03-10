-module(patience_manager).
-behaviour(gen_server).

%% API
-export([start_link/0,
         test/0,
         draw_cards/0
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

test() ->
    gen_server:call(?MODULE, {test}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
    deck_manager:shuffle_deck(),
    {ok, #state{}}.

handle_call({test}, _From, State) ->
    Reply = deck_manager:show_deck(),
    {reply, Reply, State};
handle_call({draw_cards}, _From, State) ->
    case deck_manager:deck_size() of
        N when N > 3 ->
            Cards = draw_four_cards(),
            Piles = State#state.piles,
            NewPiles = pile_util:add_one_card_to_each_pile(Cards, Piles),
            {reply, NewPiles, State#state{piles = NewPiles}};
        N when N =< 3 ->
            {reply, "No more cards left in the deck.", State}
    end.

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
