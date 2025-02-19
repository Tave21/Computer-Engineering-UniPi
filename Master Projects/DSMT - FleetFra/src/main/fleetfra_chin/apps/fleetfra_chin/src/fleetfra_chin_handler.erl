-module(fleetfra_chin_handler).
-behaviour(cowboy_handler).
-author("SaveMos").

-export([
    init/2 ,
    process_request/1 ,
    parse_json/1 ,
    build_response/1, build_response/3,
    send_update_to_other_player/3
]).

%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles incoming HTTP requests for starting a game or making a move.
%% Parses the request body, extracts the game information, and calls the appropriate game logic functions.
%% Created : 01. feb 2025 09:53
%% @param Req The HTTP request object.
%% @param State The state of the Cowboy handler.
%% @returns {ok, Req2, State} The updated request and state.
%% @end
%%==============================================================================%%
init(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Response = process_request(Body),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1),
    {ok, Req2, State}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Processes the request, which is a JSON object representing a game action.
%%
%% @param Body The raw JSON body received from a client request.
%%
%% @returns A JSON-encoded response with appropriate status messages, including:
%%   - "OK: Game started" if a new game is successfully initialized.
%%   - "OK: Turn changed" if the turn is successfully switched.
%%   - "ERROR: Game not found" if the game ID does not exist.
%%   - "Invalid move" or other move-related errors based on the game logic.
%%   - "VICTORY" or "DEFEAT" if the game ends.
%%   - "Unknown request type" if the request type is invalid.
%% @end
%%-------------------------------------------------------------------
process_request(Body) ->
    ParsedJson = parse_json(Body),
    GameID = maps:get(<<"game_id">>, ParsedJson),
    TypeRequest = maps:get(<<"type_request">>, ParsedJson),

    case TypeRequest of
        <<"start_game">> ->
            Player1 = maps:get(<<"player1">>, ParsedJson),
            Player2 = maps:get(<<"player2">>, ParsedJson),
            Battlefield1 = maps:get(<<"player1_battlefield">>, ParsedJson),
            Battlefield2 = maps:get(<<"player2_battlefield">>, ParsedJson),
            fleetfra_game:start_game(GameID, {Player1, Player2, Battlefield1, Battlefield2}),
            build_response(<<"OK: Game started">>);

        <<"start_game_client">> ->
            Player = maps:get(<<"player">>, ParsedJson),
            Battlefield = maps:get(<<"player_battlefield">>, ParsedJson),
            fleetfra_game:start_game_client(GameID, {Player, Battlefield}),
            build_response(<<"OK: Game started">>);

        <<"change_turn">> ->
            case fleetfra_game:change_turn(GameID) of
                {ok, proceed} -> build_response(<<"OK: Turn changed">>);
                {error, game_not_found} -> build_response(<<"ERROR: Game not found">>)
            end;

        <<"get_game_info">> ->
            case fleetfra_game:get_game_info(GameID) of
                {ok, JsonResponse} -> JsonResponse;
                {error, game_not_found} -> build_response(<<"ERROR: Game not found">>)
            end;

        <<"make_move">> ->
            Player = maps:get(<<"player">>, ParsedJson),
            Move = maps:get(<<"move">>, ParsedJson),
            Row = maps:get(<<"row">>, Move),
            Col = maps:get(<<"col">>, Move),

            case fleetfra_game:make_move(GameID, {Player, {Row, Col}}) of
                {ok, NewValue} ->
                    build_response(<<"OK: Move accepted [", (utility:to_binary(NewValue))/binary, "]">>);

                {error, invalid_move} ->
                    build_response(<<"Invalid move">>);

                {error, out_of_bound_coordinates} ->
                    build_response(<<"INVALID MOVE: Out of bound coordinates">>);

                {error, not_integer} ->
                    build_response(<<"INVALID MOVE: Coordinates must be integers">>);

                {error, not_your_turn} ->
                    build_response(<<"TURN ERROR: Not your turn">>);

                {error, player_not_found} ->
                    build_response(<<"ERROR: Player not found">>);

                {error, game_not_found} ->
                    build_response(<<"ERROR: Game not found">>);

                {error, game_not_initiated} ->
                    build_response(<<"ERROR: Game found but not initiated">>);

                {fin, winner} ->
                    build_response(<<"VICTORY">>);

                {fin, loser} ->
                    build_response(<<"DEFEAT">>)
            end;

        _ -> build_response(<<"Unknown request type">>)
    end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Parses the incoming JSON (using JSX) request body and returns a map.
%% @param Body The JSON body of the request.
%% @returns A map representing the parsed JSON.
%% @end
%%-------------------------------------------------------------------
parse_json(Body) ->
    jsx:decode(Body, [{return_maps, true}]).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Builds a JSON response string to send back to the client.
%% @param Message The message to include in the response.
%% @returns The JSON-encoded response.
%% @end
%%-------------------------------------------------------------------

build_response(Message) ->
    decode_json(Message).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Builds a JSON response string to send back to both the players.
%% @param Message The message to include in the response.
%% @param GameID The unique gam identifier.
%% @param CurrentPlayer The current playing player.
%% @returns The JSON-encoded response.
%% @end
%%-------------------------------------------------------------------

build_response(Message, GameID, WaitingPlayer) ->
    Response = decode_json(Message),
    send_update_to_other_player(GameID , WaitingPlayer , Response),
    Response.


%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Decode a MAP into a JSON message using the JSX library.
%% @param Message The message to include in the response.
%% @returns The JSON-encoded message.
%% @end
%%-------------------------------------------------------------------

decode_json(Message) ->
    jsx:encode(#{<<"message">> => Message}).

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Sends a game update to the opponent player.
%% @param GameID The unique gam identifier.
%% @param WaitingPlayer The target player.
%% @param GameState The current game state.
%% @returns Nothing.
%% @end
%%------------------------------------------------------------------------------

send_update_to_other_player(GameID, WaitingPlayer, Response) ->
    Name = utility:concat_game_player(GameID, WaitingPlayer),
    case erlang:whereis(Name) of
        undefined ->
            {do_not_exists};
        Pid ->
            fleetfra_chin_user_handler:send_message(Name, Response),
            Pid ! {game_update, Response},
            {ok}
    end.
