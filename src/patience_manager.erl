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
    {reply, "No more cards left in the deck.", State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
