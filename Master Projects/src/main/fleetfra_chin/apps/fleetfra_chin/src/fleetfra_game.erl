-module(fleetfra_game).
-export([start_game/2 ,start_game_client/2, make_move/2 , change_turn/1 , get_game_info/1]).
-author("SaveMos").
%%===============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Starts a new game by storing the initial game state and the players' battlefields.
%% The game state includes the game ID, the players' names, their battlefields,
%% the current turn, and whether the game is over.
%% @param GameID The unique identifier for the game.
%% @param {Player1, Player2, Battlefield1, Battlefield2} A tuple containing the players' names and their battlefields.
%% @end
%%===============================================================================%%
-record(game, {game_id, player1 , player2, battlefields, current_turn, waiting_player ,game_over, winner, created_at , init_complete}).

start_game(GameID, {Player1, Player2, Battlefield1, Battlefield2}) ->
  %% Initialize ETS table
  init_ets(),
  PlayerAtom1 = utility:to_atom(Player1),
  PlayerAtom2 = utility:to_atom(Player2),

  %% Create the initial game state using the correct map syntax
  GameState = #game{
    game_id = GameID,
    player1 = PlayerAtom1,
    player2 = PlayerAtom2,
    battlefields = #{PlayerAtom1 => Battlefield2, PlayerAtom2 => Battlefield1},
    current_turn = PlayerAtom1,
    waiting_player = PlayerAtom2,
    game_over = false,
    winner = none,
    created_at = erlang:system_time(second),  % Timestamp of creation in seconds.
    init_complete = true
  },

  % Store the game state in the ETS.
  game_state_manager:put_game_state(GameID, GameState),
  {ok, GameState}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Starts a new game by storing the initial game state and the players' battlefields.
%% The starting procedure is composed by two steps.
%% The first step starts when one of the player send the "start_game_client" request.
%% In the first step the initial structure is created and partially filled.
%% The second step starts when the other player send the "start_game_client" request.
%% In the second step the  structure is filled with the remnants information.
%% The structure represents the game state and it includes the game ID, the players' names, their battlefields,
%% the current turn, and whether the game is over and other information.
%% @param GameID The unique identifier for the game.
%% @param {Player, Battlefield} A tuple containing the player names and his battlefield.
%% @end
%%-------------------------------------------------------------------

start_game_client(GameID, {Player, Battlefield}) ->
  init_ets(), %% Initialize the ETS table.
  PlayerAtom = utility:to_atom(Player), % The player that made this request.

  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      % Phase 2 - The second player send the final information.
      NewBattlefields = maps:put(PlayerAtom, Battlefield, GameState#game.battlefields),
      Player1 = GameState#game.player1,
      Player2 = PlayerAtom,
      Battlefield1 = maps:get(Player1, NewBattlefields),
      Battlefield2 = maps:get(Player2, NewBattlefields),
      % Swap the battlefields.
      % Battlefield1 is the field where Player1 shoots.
      SwappedBattlefields = NewBattlefields#{
        Player1 => Battlefield2,
        Player2 => Battlefield1
      },
      NewGameState = GameState#game{
        battlefields = SwappedBattlefields,
        player2 = PlayerAtom,
        waiting_player = PlayerAtom,
        game_over = false,
        winner = none,
        created_at = erlang:system_time(second),
        init_complete = true
      },
      game_state_manager:put_game_state(GameID, NewGameState),  % Store the game state in the ETS.
      {ok, NewGameState};

    {error, not_found} ->
      % Phase 1 - The first player sends the initial information.
      GameState = #game{
        game_id = GameID,
        player1 = PlayerAtom,
        player2 = undefined,
        battlefields = #{PlayerAtom => Battlefield},
        current_turn = PlayerAtom,
        waiting_player = undefined,
        game_over = false,
        winner = none,
        created_at = erlang:system_time(second),
        init_complete = false
      },
      game_state_manager:put_game_state(GameID, GameState),  % Store the game state in the ETS.
      {ok, GameState}
  end.


%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Initialize the ETS, use it before inserting game states.
%% @returns ok if the ets has been initialized.
%% @end
%%-------------------------------------------------------------------
init_ets() ->
  case ets:info(game_state_table) of
    undefined -> ets:new(game_state_table, [named_table, public, set]);
    _ -> ok
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Return a JSON with all the information about a certain game.
%% @param GameID The unique identifier for the game.
%% @returns {ok, JsonResponse} The current json-encoded game state.
%%          {error, game_not_found} if the game does not exists.
%% @end
%%-------------------------------------------------------------------

get_game_info(GameID) ->
  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      JsonResponse = game_state_to_json(GameState),
      {ok, JsonResponse};
    {error, not_found} ->
      {error, game_not_found}
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Returns a JSON with all the information about a certain game.
%%
%% @param GameState The game structure, which contains various attributes like game ID, players,
%%                  battlefields, current turn, etc.
%%
%% @returns
%%   A JSON-encoded map with the game's information, including:
%%   - game_id: The unique identifier of the game
%%   - player1: The first player
%%   - player2: The second player
%%   - battlefields: The map of battlefields converted to JSON
%%   - current_turn: The player whose turn it is
%%   - waiting_player: The player who is waiting for their turn
%%   - game_over: Boolean indicating whether the game is over
%%   - winner: The winner of the game (if any)
%%   - created_at: The timestamp when the game was created
%%   - init_complete: Boolean indicating if the game setup is complete
%% @end
%%-------------------------------------------------------------------
game_state_to_json(#game{
  game_id = GameID,
  player1 = Player1,
  player2 = Player2,
  battlefields = Battlefields,
  current_turn = CurrentTurn,
  waiting_player = WaitingPlayer,
  game_over = GameOver,
  winner = Winner,
  created_at = CreatedAt,
  init_complete = InitComplete
}) ->

  Battlefield1 = maps:get(Player1, Battlefields),
  Battlefield2 = maps:get(Player2, Battlefields),

  SwappedBattlefields = Battlefields#{
    Player1 => Battlefield2,
    Player2 => Battlefield1
  },

  JsonMap = #{
    <<"game_id">> => GameID,
    <<"player1">> => Player1,
    <<"player2">> => Player2,
    <<"battlefields">> => battlefields_to_json(SwappedBattlefields),
    <<"current_turn">> => CurrentTurn,
    <<"waiting_player">> => WaitingPlayer,
    <<"game_over">> => GameOver,
    <<"winner">> => Winner,
    <<"created_at">> => CreatedAt,
    <<"init_complete">> => InitComplete
  },
  jsx:encode(JsonMap).  % Encodes the map as a JSON string

%%-------------------------------------------------------------------
%% @doc
%% Converts a map of battlefields into a JSON representation.
%% @param Battlefields A map of battlefields (each battlefield associated with a player).
%% @returns
%%   A map where each battlefield is converted into its JSON form.
%% @end
%%-------------------------------------------------------------------
battlefields_to_json(Battlefields) ->
  maps:map(fun(_, BF) -> battlefield_to_json(BF) end, Battlefields).

%%-------------------------------------------------------------------
%% @doc
%% Converts a single battlefield into a JSON representation.
%% @param Battlefield The battlefield structure, which can be either a map or a list.
%% @returns
%%   If the battlefield is a map, it returns the map's values.
%%   If the battlefield is a list, it returns a list of JSON-encoded cells.
%% @end
%%-------------------------------------------------------------------
battlefield_to_json(Battlefield) when is_map(Battlefield) ->
  maps:values(Battlefield);  % Returns the values of the map (battlefield elements)

battlefield_to_json(Battlefield) when is_list(Battlefield) ->
  [cell_to_json(Cell) || Cell <- Battlefield].  % Converts each cell in the list to JSON

%%-------------------------------------------------------------------
%% @doc
%% Converts a single cell of the battlefield into a JSON representation.
%% @param Cell A map containing row, column, and value of the battlefield cell.
%% @returns
%%   A map with keys: row, col, value, representing the cell in JSON format.
%% @end
%%-------------------------------------------------------------------
cell_to_json(#{<<"row">> := Row, <<"col">> := Col, <<"value">> := Value}) ->
  #{<<"row">> => Row, <<"col">> => Col, <<"value">> => Value}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Swap the current turn of the target game.
%% @param GameID The unique identifier for the game.
%% @returns {ok, proceed} if the turn has been changed.
%%          {error, game_not_found} if the game does not exists.
%% @end
%%-------------------------------------------------------------------

change_turn(GameID) ->
  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      %% Use update_game_state to update the GameState
      NewGameState = update_game_state_turn(GameState),
      game_state_manager:put_game_state(GameID, NewGameState),
      {ok, proceed};
    {error, not_found} ->
      {error, game_not_found}
    end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Makes a move in the game by updating the corresponding battlefield.
%% It checks if the move is valid and updates the game state accordingly.
%%
%% @param GameID The unique identifier for the game.
%% @param {Player, {Row, Col}} A tuple containing the player's name and the coordinates of the move.
%%
%% @returns
%%   {ok, NewValue, WaitingPlayerAtom} if the move is valid and the game continues,
%%   {fin, winner, WaitingPlayerAtom} if the player wins the game,
%%   {fin, loser} if the game is over and the player loses,
%%   {error, out_of_bound_coordinates} if the move is outside the bounds of the game board,
%%   {error, not_integer} if the coordinates are not valid integers,
%%   {error, game_not_initiated} if the game has not been initiated yet,
%%   {error, not_your_turn} if it is not the player's turn to move,
%%   {error, player_not_found} if the player is not found in the game state,
%%   {error, game_not_found} if the game does not exist.
%% @end
%%-------------------------------------------------------------------
make_move(GameID, {Player, {Row, Col}}) ->
  PlayerAtom = utility:to_atom(Player),
  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      case GameState#game.current_turn of
        PlayerAtom ->
          %WaitingPlayerAtom = GameState#game.waiting_player,
          %io:format("OK: ~p can play!~n" , [PlayerAtom]),
          case maps:find(PlayerAtom, GameState#game.battlefields) of
            {ok, PlayerBattlefield} ->
              %io:format("OK: ~p battlefield found!~n"  , [PlayerAtom]),
              case GameState#game.init_complete of
                true ->
                  case GameState#game.game_over of
                    true ->
                      % The game is already ended, so the other player won.
                      case GameState#game.winner of
                        PlayerAtom ->
                          {fin, winner}; % Player wins
                        _ ->
                          game_state_manager:delete_game_state(GameID),
                          %websocket_manager:remove_pid(GameID, Player),
                          {fin, loser} % Player loses
                      end;
                    false ->
                      % The game is not over yet.
                      case check_move_coordinates(Row, Col) of
                        {ok , _} ->
                          %% Update the battlefield with the move
                          {UpdatedBattlefield, NewValue} = update_battlefield(PlayerBattlefield, Row, Col),
                          %% Use update_game_state to update the GameState
                          NewGameState = update_game_state(GameState, Player, UpdatedBattlefield , NewValue),
                          %% Save the new game state
                          game_state_manager:put_game_state(GameID, NewGameState),
                          case NewGameState#game.game_over of
                            true ->
                              case NewGameState#game.winner of
                                PlayerAtom ->
                                  {fin, winner}; % Player wins
                                _ ->
                                  game_state_manager:delete_game_state(GameID),
                                  %websocket_manager:remove_pid(GameID, Player),
                                  {fin, loser} % Player loses
                              end;
                            _ ->
                              {ok, NewValue} % Game continues
                          end;
                        {error , out_of_bounds} ->
                          {error, out_of_bound_coordinates}; % Move out of bounds
                        {error, not_integer} ->
                          {error, not_integer} % Invalid coordinate type
                      end
                  end;
                false ->
                  {error , game_not_initiated} % Game has not been initiated
              end;
            error ->
              {error, player_not_found} % Player not found in the battlefield
          end;
        _ ->
          case GameState#game.waiting_player of
            PlayerAtom ->
              {error, not_your_turn}; %% It's not their turn.
            _ ->
              {error, player_not_found} % The player does not exist in the game state
          end
      end;
    {error, not_found} ->
      {error, game_not_found} % Game not found
  end.


%%%-------------------------------------------------------------------
%%% @doc
%%% Validates the move coordinates to ensure they are within the valid range.
%%% Additionally, it checks that Row and Col are integers.
%%% The battlefield dimension is retrieved dynamically using `get_battlefield_dimension/0`.
%%%
%% @param Row The row index of the move (integer).
%% @param Col The column index of the move (integer).
%%
%% @returns {ok, proceed} if the move is valid.
%%%          {error, invalid_input} if Row or Col is not an integer.
%%%          {error, out_of_bounds} if the move is outside the allowed range.
%%% @end
%%%-------------------------------------------------------------------

check_move_coordinates(Row, Col) ->
  Dim = fleetfra_chin_configuration:get_battlefield_dimension(),

  case {is_integer(Row), is_integer(Col)} of
    {false, _} -> {error, not_integer};  %% Row is not an integer.
    {_, false} -> {error, not_integer};  %% Col is not an integer.
    {true, true} ->
      if
        Row < 1 orelse Row >= Dim orelse Col < 1 orelse Col >= Dim ->
          %% If the row or column is outside the battlefield range, return an error.
          {error, out_of_bounds};
        true ->
          %% If the move is valid, return a success tuple.
          {ok, proceed}
      end
  end.


%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Updates the battlefield matrix with a new value at the specified row and column.
%% This function assumes the battlefield is a 10x10 matrix represented as a list of lists.
%% @param Battlefield The current battlefield.
%% @param Row The row index of the cell to update.
%% @param Col The column index of the cell to update.
%% @param NewValue The new value to set at the specified cell.
%% @returns The updated battlefield and the new value of the hit cell.
%% @end
%%-------------------------------------------------------------------

update_battlefield(Battlefield, Row, Col) ->
  %% Update the battlefield
  NewBattlefield = [ case Cell of
                       #{<<"row">> := RowIndex, <<"col">> := ColIndex}
                         when RowIndex == Row andalso ColIndex == Col ->
                         %% Determine the new value of the cell
                         NewValue0 = case Cell of
                                       #{<<"value">> := -1} -> 3;  %% Untouched water -> hit water
                                       #{<<"value">> := 0} -> 3;  %% Untouched water -> hit water
                                       #{<<"value">> := 1} -> 2;  %% Ship not hit -> ship hit
                                       #{<<"value">> := 2} -> 2;  %% Ship already hit -> remains unchanged
                                       #{<<"value">> := 3} -> 3;  %% Water already hit -> remains unchanged
                                       _ -> 0  %% Default case: untouched water
                                     end,
                         %% Update the cell with the new value
                         Cell#{<<"value">> => NewValue0};
                       _ ->
                         Cell  %% Leave other cells unchanged
                     end || Cell <- Battlefield],
  %% Retrieve the new value of the updated cell
  NewValue = case lists:filter(
    fun(Cell) ->
      case Cell of
        #{<<"row">> := RowIndex, <<"col">> := ColIndex} ->
          RowIndex == Row andalso ColIndex == Col;
        _ ->
          false
      end
    end, NewBattlefield) of
               [UpdatedCell|_] -> maps:get(<<"value">>, UpdatedCell);
               [] -> undefined
             end,
  {NewBattlefield, NewValue}.


%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Updates the game state after a move. Also check if the game is over.
%% @param GameState The current game state.
%% @param Player The player who made the move.
%% @param NewBattlefield The updated battlefield of the player.
%% @returns The updated game state.
%% @end
%%-------------------------------------------------------------------
update_game_state(GameState, Player, NewBattlefield, NewValue) ->
  %% Convert Player to an atom if it's a binary
  PlayerAtom = utility:to_atom(Player),

  %% Update the battlefields map with the correct key (PlayerAtom)
  NewBattlefields = maps:put(PlayerAtom, NewBattlefield, GameState#game.battlefields),

  %% Get the current player's battlefield
  PlayerBattlefield = maps:get(PlayerAtom, NewBattlefields),

  %% Check if the game is over
  GameOver = check_game_over(PlayerBattlefield),

  %% Determine the winner if the game is over
  Winner = case GameOver of
             true -> PlayerAtom;
             _ -> none
           end,

  case {NewValue,GameOver} of
    {2 , false} ->
      % Ship hit, the turn does not change.
      NewTurn = GameState#game.current_turn,
      OldTurn = GameState#game.waiting_player;
    {3 , false} ->
      % Water hit, the turn does change.
      NewTurn = GameState#game.waiting_player,
      OldTurn = GameState#game.current_turn;
    {_ , true} ->
      % The player who just made the move have won.
      NewTurn = GameState#game.waiting_player,
      OldTurn = GameState#game.current_turn;
    _ ->
      % Default, the turn change.
      NewTurn = GameState#game.waiting_player,
      OldTurn = GameState#game.current_turn
  end,
  %% Return the updated GameState
  GameState#game{battlefields = NewBattlefields, current_turn = NewTurn , waiting_player = OldTurn, game_over = GameOver, winner = Winner, created_at = erlang:system_time(second)}.


%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Change the current turn of the match.
%% @param GameState The current game state.
%% @returns The updated game state.
%% @end
%%-------------------------------------------------------------------

update_game_state_turn(GameState) ->
  PlayerAtom = GameState#game.current_turn,
  NewTurn = GameState#game.waiting_player,
  %% Return the updated GameState
  GameState#game{current_turn = NewTurn , waiting_player = PlayerAtom, created_at = erlang:system_time(second)}.


%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Checks if the game is over, meaning all the ships of one player have been hit.
%% @param GameState The current game state.
%% @returns An atom, true if the game is over, false otherwise.
%% @end
%%-------------------------------------------------------------------

check_game_over(Battlefield) ->
  case lists:member(true, [case Cell of
                             #{<<"value">> := 1} -> true;  %% If the cell is an un-hit ship.
                             _ -> false %% If the cell is an hit ship or water.
                           end || Cell <- Battlefield]) of
    true -> false;  %% If there's at least one un-hit ship, the game is not over.
    false -> true   %% If all ships are hit, the game is over.
  end.
