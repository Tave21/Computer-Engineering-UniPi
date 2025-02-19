%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% WebSocket Manager module for handling WebSocket processes storage using ETS.
%% This module provides API functions for storing, retrieving, and deleting WebSocket processes.
%% It also includes periodic cleanup of expired WebSocket processes.
%% @end
%% Created : 29. Jan 2025 15:03
%%==============================================================================%%

-module(websocket_manager).
-author("Saverio").

%% API
-export([start_link/0, store_pid/2, get_opponent_pid/3, remove_pid/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Define ETS table name
-define(ETS_TABLE, websocket_registry).

%%%===================================================================
%%% API functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Starts the websocket manager.
%% @returns {ok, pid} if the process is successfully started.
%% @end
%%-------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Stores the PID of a WebSocket process associated with a GameID and PlayerID.
%% @param GameID The unique identifier of the game.
%% @param PlayerID The unique identifier of the player.
%% @returns {noreply} after storing the PID in ETS.
%% @end
%%-------------------------------------------------------------------
store_pid(GameID, PlayerID) ->
  io:format("Memorizing PID for GameID: ~p, PlayerID: ~p~n", [GameID, PlayerID]),
  gen_server:cast(?MODULE, {store_pid, GameID, PlayerID, self()}).

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Retrieves the PID of the opponent player.
%% @param GameID The unique identifier of the game.
%% @param PlayerID The unique identifier of the player.
%% @param WaitingPlayer The identifier of the opponent player.
%% @returns {ok, OpponentPID} if the opponent's PID is found, or {error, not_found} if not.
%% @end
%%-------------------------------------------------------------------
get_opponent_pid(GameID, PlayerID, WaitingPlayer) ->
  gen_server:call(?MODULE, {get_opponent_pid, GameID, PlayerID, WaitingPlayer}).

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Removes a PID from the registry when a connection is closed.
%% @param GameID The unique identifier of the game.
%% @param PlayerID The unique identifier of the player.
%% @returns {noreply} after removing the PID from ETS.
%% @end
%%-------------------------------------------------------------------
remove_pid(GameID, PlayerID) ->
  gen_server:cast(?MODULE, {remove_pid, GameID, PlayerID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Initializes the WebSocket Manager process. It creates the ETS table
%% for storing WebSocket process information.
%% @param [] No parameters are needed for initialization.
%% @returns {ok, State} where State is an empty map.
%% @end
%%-------------------------------------------------------------------
init([]) ->
  ets:new(?ETS_TABLE, [set, public, named_table]),
  {ok, #{}}.

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles the request to retrieve the opponent's PID from the ETS table.
%% @param {get_opponent_pid, GameID, PlayerID, OpponentID} The request tuple with GameID, PlayerID, and OpponentID.
%% @returns {ok, OpponentPID} if the opponent's PID is found, or {error, not_found} if not.
%% @end
%%-------------------------------------------------------------------
handle_call({get_opponent_pid, GameID, PlayerID , OpponentID}, _From, State) ->
  PlayerID_Str = utility:to_str(PlayerID),
  GameID_str = utility:to_str(GameID),
  io:format("Searching for opponent PID for GameID: ~p, PlayerID: ~p~n", [GameID_str, PlayerID_Str]),
  case ets:lookup(?ETS_TABLE, {GameID_str, PlayerID_Str}) of
    [{ok, PID}] ->
      OpponentID_Str = utility:to_str(OpponentID),
      io:format("~p Looking for opponent: ~p~n", [PID , OpponentID_Str]),
      case ets:lookup(?ETS_TABLE, {GameID_str, OpponentID_Str}) of
        [{ok, OpponentPID}] ->
          io:format("Found: ~p~n", [OpponentID]),
          {reply, {ok, OpponentPID}, State};
        [] -> {reply, {error, not_found}, State}
      end;
    [] -> {reply, {error, not_found}, State}
  end.

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles the request to store a PID for a GameID and PlayerID in the ETS table.
%% @param {store_pid, GameID, PlayerID, PID} The request tuple with GameID, PlayerID, and the PID of the WebSocket process.
%% @returns {noreply} after storing the PID in ETS.
%% @end
%%-------------------------------------------------------------------
handle_cast({store_pid, GameID, PlayerID, PID}, State) ->
  ets:insert(?ETS_TABLE, {{GameID, PlayerID}, PID}),
  {noreply, State};

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles the request to remove a PID for a GameID and PlayerID from the ETS table.
%% @param {remove_pid, GameID, PlayerID} The request tuple with GameID and PlayerID to be removed.
%% @returns {noreply} after removing the PID from ETS.
%% @end
%%-------------------------------------------------------------------
handle_cast({remove_pid, GameID, PlayerID}, State) ->
  ets:delete(?ETS_TABLE, {GameID, PlayerID}),
  {noreply, State}.

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Terminates the WebSocket Manager process and deletes the ETS table.
%% @param _Reason The reason for termination.
%% @param _State The current state of the gen_server process.
%% @returns ok after cleaning up the ETS table.
%% @end
%%-------------------------------------------------------------------
terminate(_Reason, _State) ->
  ets:delete(?ETS_TABLE),
  ok.

%%-------------------------------------------------------------------
%% @author Saverio
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles code upgrades in the WebSocket Manager.
%% @param _OldVsn The old version of the module.
%% @param State The current state of the gen_server process.
%% @param _Extra Any additional arguments.
%% @returns {ok, State} after completing the code change handling.
%% @end
%%-------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
