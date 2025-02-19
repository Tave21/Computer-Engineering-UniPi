-module(fleetfra_chin_ws_handler).
-behaviour(cowboy_websocket).
-author("SaveMos").
-export([
  init/2,
  websocket_init/1,
  websocket_handle/2,
  websocket_info/3,
  websocket_info/2,
  terminate/3,
  extract_params/1, register_websocket_process/2
]).

%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% WebSocket handler for FleetFra Chin.
%% This module manages WebSocket connections, processing incoming messages
%% and delegating request handling to `fleetfra_chin_handler`.
%% @end
%%==============================================================================%%

%-record(client, {pid}).

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Called when the WebSocket connection is initialized.
%% @param State The state of the WebSocket connection.
%% @returns A tuple indicating success and returning the state.
%% @end
%%------------------------------------------------------------------------------

init(Req, State) ->
  io:format("WebSocket Starting~n"),
  %case extract_params(Req) of
    %{GameID, PlayerID} -> pass;
      %%fleetfra_chin_user_handler:start_link(GameID, PlayerID);
      %register_websocket_process(GameID, PlayerID);
    %{error, _} -> pass end,
  {cowboy_websocket, Req, State
    %,#{idle_timeout => infinity}
  }.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Register the current process with the name "GameID_PlayerID".
%% @param GameID The unique identifier for the game.
%% @param Player The player who made the request.
%% @returns Nothing.
%% @end
%%------------------------------------------------------------------------------

register_websocket_process(GameID, PlayerID) ->
  Name = utility:concat_game_player(GameID, PlayerID),
  case erlang:whereis(Name) of
    undefined ->
      pass;
    _ ->
      erlang:unregister(Name)
  end,
  erlang:register(Name, erlang:self()).

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Called when the WebSocket connection is initialized.
%% @param State The state of the WebSocket connection.
%% @returns A tuple indicating success and returning the state.
%% @end
%%------------------------------------------------------------------------------
websocket_init(State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Helper function to extract parameters from WebSocket request
%% Extracts the GameID and the PlayerID from the WebSocket request URI.
%% @param Req The request.
%% @returns A tuple indicating success and returning the state.
%% @end
%%------------------------------------------------------------------------------

extract_params(Req) ->
  QueryString = cowboy_req:qs(Req), % Get the query parameters.
  QueryStringStr = binary:bin_to_list(QueryString),
  parse_query_string(QueryStringStr). % Extract the parameters.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Function to parse the QueryString.
%% @param QueryString The query string.
%% @returns The two parameters GameID and PlayerID.
%% @end
%%------------------------------------------------------------------------------

parse_query_string(QueryString) ->
  Pairs = string:tokens(QueryString, "&"), % Query split by '&'.
  % Key-Value parsing (game_id=...&player=...)
  lists:foldl(fun(Pair, Acc) ->
    case string:split(Pair, "=") of
      [Key, Value] when Key == "game_id" -> {Value, element(2, Acc)};
      [Key, Value] when Key == "player" -> {element(1, Acc), Value};
      _ -> Acc
    end
              end, {undefined, undefined}, Pairs).

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles incoming WebSocket messages.
%% @param Msg The received WebSocket message (expected to be in JSON format).
%% @param State The current state of the WebSocket connection.
%% @returns A tuple containing the response message and updated state.
%% @end
%%------------------------------------------------------------------------------
websocket_handle({text, Msg}, State) ->
  %io:format("Received message: ~s~n", [Msg]),
  Response = fleetfra_chin_handler:process_request(Msg), %% Process the request using the game logic handler.

  {reply, {text, Response}, State};   %% Send the response back to the WebSocket client.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles unexpected or unsupported WebSocket data.
%% @param _Data The received data that is not handled explicitly.
%% @param State The current state.
%% @returns A tuple indicating that the connection remains open.
%% @end
%%------------------------------------------------------------------------------
websocket_handle(_Data, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles WebSocket messages sent from Erlang processes.
%% This can be used to push messages from the server to the client.
%% @param Info The message received from another process.
%% @param State The current state.
%% @returns A tuple indicating that the connection remains open.
%% @end
%%------------------------------------------------------------------------------
websocket_info({game_update, Response}, Req, State) ->
  case jsx:is_jsonable(Response) of
    true -> Message = jsx:encode(Response);
    false -> Message = "{\"error\": \"invalid json\"}"
  end,
  {reply, {text, Message}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_info(_Info, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Called when the WebSocket connection is closed.
%% @param Reason The reason for termination.
%% @param _Req The WebSocket request.
%% @param _State The final state before termination.
%% @returns `ok` to indicate successful cleanup.
%% @end
%%------------------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
  io:format("WebSocket terminating: ~p - PID: ~p ~n", [_Reason, self()]),
  ok.