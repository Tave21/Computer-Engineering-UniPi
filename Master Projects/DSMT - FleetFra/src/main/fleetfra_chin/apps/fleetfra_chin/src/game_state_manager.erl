%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Game State Manager module for handling game state storage using ETS.
%% This module provides API functions for storing, retrieving, and deleting game states.
%% It also includes periodic cleanup of expired game states.
%% @end
%% Created : 29. Jan 2025 15:03
%%==============================================================================%%

-module(game_state_manager).
-author("SaveMos").
-behaviour(gen_server).

%% API
-export([start_link/0, put_game_state/2, get_game_state/1, delete_game_state/1]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([handle_sync_call/1, verify_propagation/2]).

-record(game, {game_id, players, battlefields, current_turn, game_over, winner, created_at}).
-define(ETS_TABLE, game_state_table).

%% List of known nodes in the cluster
-define(KNOWN_NODES, ['fleetfra30@10.2.1.30', 'fleetfra29@10.2.1.29', 'fleetfra28@10.2.1.28']).

% 10.2.1.30  fleetfra30
% 10.2.1.29  fleetfra29
% 10.2.1.28  fleetfra28



%%==============================================================================%%
%% API for ETS Management
%%==============================================================================%%

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Starts the GameStateManager process.
%% @returns {ok, pid} if the process is started successfully.
%% @end
%%-------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Inserts a new game state into ETS.
%% @param GameID The unique identifier for the game.
%% @param GameState The state of the game to be inserted.
%% @returns {ok} if the game state is successfully inserted.
%% @end
%%-------------------------------------------------------------------
put_game_state(GameID, GameState) ->
  gen_server:call(?MODULE, {put, GameID, GameState}).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Retrieves a game state from ETS.
%% @param GameID The unique identifier for the game.
%% @returns {ok, GameState} if the game state is found, or {error, not_found} if not.
%% @end
%%-------------------------------------------------------------------
get_game_state(GameID) ->
  gen_server:call(?MODULE, {get, GameID}).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Deletes a game state from ETS.
%% @param GameID The unique identifier for the game.
%% @returns {ok} if the game state is successfully deleted.
%% @end
%%-------------------------------------------------------------------
delete_game_state(GameID) ->
  gen_server:call(?MODULE, {delete, GameID}).

%%==============================================================================%%
%% GenServer Callbacks
%%==============================================================================%%

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Initializes the GameStateManager process. It creates the ETS table and starts a periodic
%% cleanup process for expired game states.
%% @param [] No parameters are needed during initialization.
%% @returns {ok, State} where State is an empty map.
%% @end
%%-------------------------------------------------------------------
init([]) ->
  ets:new(?ETS_TABLE, [named_table, public, set]),
  erlang:set_cookie(node(), 'fleetfra'),

% Start the periodic cleanup process every hour
  erlang:send_after(fleetfra_chin_configuration:get_auto_clean_period(), erlang:self(), clean_old_games),
  {ok, #{}}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles the insertion of a new game state into ETS.
%% @param {put, GameID, GameState} A tuple with the GameID and the corresponding GameState.
%% @returns {ok} if the game state was successfully inserted.
%% @end
%%-------------------------------------------------------------------
handle_call({put, GameID, GameState}, _From, State) ->
  ets:insert(?ETS_TABLE, {GameID, GameState}),
  propagate_update({sync_put, GameID, GameState}),
  %timer:sleep(1000),
  %verify_propagation(GameID, GameState),
  {reply, ok, State};

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Retrieves a game state from ETS based on the GameID.
%% @param {get, GameID} The GameID for which the game state is requested.
%% @returns {ok, GameState} if the game state is found, or {error, not_found} if not.
%% @end
%%-------------------------------------------------------------------
handle_call({get, GameID}, _From, State) ->
  case ets:lookup(?ETS_TABLE, GameID) of
    [{GameID, GameState}] -> {reply, {ok, GameState}, State};
    [] -> {reply, {error, not_found}, State}
  end;

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Deletes a game state from ETS based on the GameID.
%% @param {delete, GameID} The GameID of the game state to be deleted.
%% @returns {ok} if the game state was successfully deleted.
%% @end
%%-------------------------------------------------------------------
handle_call({delete, GameID}, _From, State) ->
  ets:delete(?ETS_TABLE, GameID),
  propagate_update({sync_delete, GameID}),
  {reply, ok, State};

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles updating a game state in ETS. If the game state exists, it is overwritten.
%% @param {update, GameID, NewGameState} A tuple with the GameID and the updated GameState.
%% @returns {ok} if the game state is successfully updated, or {error, not_found} if the GameID doesn't exist.
%% @end
%%-------------------------------------------------------------------
handle_call({update, GameID, NewGameState}, _From, State) ->
  case ets:lookup(?ETS_TABLE, GameID) of
    [{GameID, _OldGameState}] ->
      ets:insert(?ETS_TABLE, {GameID, NewGameState}), % Overwrites the existing state
      {reply, ok, State};
    [] ->
      {reply, {error, not_found}, State}
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Synchronization operation, done between game state managers.
%% Handles updating a game state in ETS. If the game state exists, it is overwritten.
%% @param {sync_put, GameID, GameState} A tuple with the GameID and the updated GameState.
%% @returns {ok} if the game state is successfully updated, or {error, not_found} if the GameID doesn't exist.
%% @end
%%-------------------------------------------------------------------

handle_sync_call({sync_put, GameID, GameState}) ->
  ets:insert(?ETS_TABLE, {GameID, GameState}),
  {reply, ok};

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Synchronization operation, done between game state managers.
%% Handles deleting a game state in ETS.
%% @param {sync_delete, GameID, GameState} A tuple with the GameID and the updated GameState.
%% @returns {ok} if the game state is successfully updated, or {error, not_found} if the GameID doesn't exist.
%% @end
%%-------------------------------------------------------------------

handle_sync_call({sync_delete, GameID}) ->
  ets:delete(?ETS_TABLE, GameID),
  {reply, ok}.


%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles pushing an item to a queue stored in the state and returns the updated state.
%% @param {push, Item} The item to be added to the queue.
%% @returns The new state with the added item in the queue.
%% @end
%%-------------------------------------------------------------------
handle_cast({push, Item}, State) ->
  {noreply, [Item | State]};

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Removes the top item from the queue stored in the state.
%% @param {pop} The request to remove the top item from the queue.
%% @returns The updated state with the top item removed.
%% @end
%%-------------------------------------------------------------------
handle_cast({pop}, [_ | Tail]) ->
  {noreply, Tail};
handle_cast({pop}, []) ->
  {noreply, []}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Periodically cleans up expired game states from ETS. It checks the creation time
%% of each game state and deletes the ones that have expired.
%% @param clean_old_games The message that triggers the cleanup process.
%% @returns {noreply, State} with the state unchanged.
%% @end
%%-------------------------------------------------------------------
handle_info(clean_old_games, State) ->
  Now = erlang:system_time(second),
  ExpirationTime = fleetfra_chin_configuration:get_max_match_age(),
  % Retrieve all game states from ETS
  Games = ets:tab2list(?ETS_TABLE),
  lists:foreach(fun({GameID, GameState}) ->
    CreatedAt = GameState#game.created_at,
    case Now - CreatedAt >= ExpirationTime of
      true ->
        ets:delete(?ETS_TABLE, GameID),
        propagate_update({sync_delete, GameID});
      false -> ok
    end
                end, Games),
  % Schedule the next cleanup execution after 1 second
  erlang:send_after(fleetfra_chin_configuration:get_auto_clean_period(), erlang:self(), clean_old_games),
  {noreply, State};

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles any other messages not matching specific patterns.
%% @param _Msg The message received.
%% @returns {noreply, State} with the state unchanged.
%% @end
%%-------------------------------------------------------------------
handle_info(_Msg, State) ->
  {noreply, State}.

%%==============================================================================%%
%% Synchronization Helpers
%%==============================================================================%%

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Send a message to all the registered game state managers.
%% The update operation follow the eventual consistency paradigm.
%% @param Message is the message to send.
%% @returns {ok} Everything gone well, error otherwise.
%% @end
%%-------------------------------------------------------------------

propagate_update(Message) ->
  %io:format("Known nodes: ~p~n", [?KNOWN_NODES]),
  lists:foreach(fun(Node) ->
    % For each registered node.
    %io:format("Pinging node ~p: ~p~n", [Node, net_adm:ping(Node)]),
    case Node =/= node() of
      true -> %
        %io:format("Trying to send message to ~p~n", [Node]),
        case net_adm:ping(Node) of
          pong -> % Remote node reachable.
            %%io:format("Node ~p is reachable, sending message ~n", [Node]),
            try
              rpc:cast(Node, ?MODULE, handle_sync_call, [Message]) % Async operation.
            of
              {reply, ok} ->
                %io:format("Message sent successfully to ~p~n", [Node]),
                pass;
              true ->
                %io:format("Message sent successfully to ~p~n", [Node]),
                pass;
              {badrpc, Reason} ->
                io:format("RPC call failed to ~p with reason: ~p~n", [Node, Reason]);
              {error, Reason} ->
                io:format("RPC call failed to ~p with reason: ~p~n", [Node, Reason]);
              Unexpected ->
                io:format("Unexpected RPC result from ~p: ~p~n", [Node, Unexpected])
            catch
              error:Reason ->
                io:format("RPC failed to ~p with exception: ~p~n", [Node, Reason])
            end;
          _ -> % Remote node unreachable.
            io:format("Node ~p is not reachable~n", [Node])
        end;
      false ->
        ok % Do not send message to self.
    end
 end, ?KNOWN_NODES).


verify_propagation(GameID, ExpectedState) ->
  lists:foreach(fun(Node) ->
    case rpc:call(Node, ets, lookup, [?ETS_TABLE, GameID]) of
      [{GameID, ExpectedState}] ->
        io:format("Propagation successful on node ~p~n", [Node]);
      [] ->
        io:format("Propagation failed on node ~p (data not found)~n", [Node]);
      Other ->
        io:format("Unexpected result from node ~p: ~p~n", [Node, Other])
    end  end, ?KNOWN_NODES).


