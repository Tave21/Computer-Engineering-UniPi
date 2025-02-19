
---

# FleetFra Chin Game Application

## Overview

The **FleetFra Chin** application is an Erlang-based system that simulates a fleet battle game where two players compete against each other. The system is designed around the OTP (Open Telecom Platform) principles, utilizing supervisors for process management, and communication occurs over HTTP using the Cowboy web server.

This application supports starting a game, making moves, and receiving status updates. Players interact with the server by sending structured JSON messages to control the game flow.

## Features

- **Start a new game**: Players can initiate a game with a battlefield grid.
- **Make moves**: Players can make moves by specifying the row and column they wish to target.
- **Game status updates**: After every move, players receive updates about the game's progress (win, loss, etc.).

## Application Structure

The application consists of several modules, each responsible for different tasks:

1. **`fleetfra_chin_sup`**: The top-level supervisor that manages the application's processes.
2. **`fleetfra_chin_configuration`**: Contains configuration constants such as port numbers and cleaning periods.
3. **`fleetfra_chin_app`**: The main entry point that starts the application.
4. **`fleetfra_chin`**: Core game logic handling player interactions and game state.

### Dependencies

- **Erlang/OTP**: The programming environment used to develop the application.
- **Cowboy HTTP Server**: Handles HTTP requests from clients (players).

Ensure you have Erlang installed before proceeding with the setup. You can find installation instructions on the [official Erlang website](https://www.erlang.org/downloads).

## Installation and Setup

To set up and run the **FleetFra Chin** application, follow these steps:

### 1. Install Erlang

First, ensure that Erlang is installed. You can download it from [Erlang's website](https://www.erlang.org/downloads). Make sure the version is compatible with the application.
```bash
apt update
apt install -y erlang
```

### 2. Install rebar3
```bash
apt install -y rebar3
```
### 3. Clone the Repository

Clone the **FleetFra Chin** repository to your local machine:

```bash
git clone https://github.com/SaveMos/FleetFra
cd fleetfra_chin
```

### 4. Compile and Run the Application

After cloning the repository, compile the Erlang application using `rebar3`:

```bash
rebar3 compile
```
This will download and compile the necessary dependencies.

```bash
rebar3 shell
```
This will start the web server.

### 5. Access the Web Server

By default, the web server listens on port **8080**. You can interact with the server via HTTP requests.

## Message Types

The application communicates using JSON messages. Below are the two main types of messages exchanged between the client (player) and the server.

### 1. **Start Game Request**

This message is used to initiate a new game between two players. It provides the game ID, player names, and the initial state of both players' battlefields.

#### Example Start Game Message:

```json
{
  "game_id": "game123",
  "battlefield": [
    {"row": 0, "value": 0, "col": 0},
    {"row": 0, "value": 0, "col": 1},
    {"row": 1, "value": 0, "col": 0},
    {"row": 1, "value": 0, "col": 1}
  ],
  "player": "Player1",
  "type_request": "start_game"
}
```

#### Fields:

- **game_id**: A unique identifier for the game.
- **player1_battlefield** and **player2_battlefield**: The initial battlefield grids for the players. Each battlefield is represented by a list of positions, where `value` is `0` (empty) or `1` (occupied).
- **player1** and **player2**: The names of the two players.
- **type_request**: Always `"start_game"` when initiating a new game.

### 2. **Make Move Request**

This message is sent by a player to make a move in the game, specifying the row and column where they want to attack.

#### Example Make Move Message:

```json
{
  "move": {
    "col": 5,
    "row": 5
  },
  "type_request": "make_move",
  "player": "Player1",
  "game_id": "game123"
}
```

#### Fields:

- **move**: Specifies the coordinates of the move (`row` and `col`), both of which range from `0` to `9`.
- **type_request**: Always `"make_move"` when making a move.
- **player**: The name of the player making the move.
- **game_id**: The unique identifier of the game.

### 3. **Game Status Response**

The server will respond to each move with a message indicating the current status of the game. The status messages can be:

- **Victory**: The player has won the game.
- **Defeat**: The player has lost the game.
- **Game Not Found**: The specified game ID is invalid or the game does not exist.

#### Example Victory Response:

```json
{
  "message": "VICTORY"
}
```

#### Example Defeat Response:

```json
{
  "message": "DEFEAT"
}
```

#### Example Game Not Found Response:

```json
{
  "message": "Game not found"
}
```

## Supervisor Architecture

The system is designed using the **supervisor** pattern, which is a key feature of the Erlang/OTP model. The supervisor is responsible for managing child processes and ensuring fault tolerance.

The supervisor uses the **`one_for_all`** strategy, meaning that if any child process fails, all child processes will be terminated.

### Supervisor Flags:

```erlang
SupFlags = #{strategy => one_for_all,
             intensity => 0,
             period => 1}.
```

- **strategy**: `one_for_all` ensures that the failure of one child process results in the termination of all children.
- **intensity**: Set to `0` (not used in this case).
- **period**: Set to `1` second (not used in this case).

### Starting the Supervisor:

The supervisor is started with the following command:

```erlang
start_link() -> 
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).
```

## Configuration Constants

The application has configuration constants defined in the `fleetfra_chin_configuration` module. These constants are used for configuration purposes, such as setting the server port and defining cleaning periods.

### Available Constants:

- **get_port/0**: Returns the port number for the web server (default: 8080).
- **get_auto_clean_period/0**: Defines the period for auto-cleaning (default: 1 hour).
- **get_max_match_age/0**: Defines the maximum age for a match (default: 24 hours).

## Conclusion

The **FleetFra Chin** application provides a simple yet powerful system for simulating a fleet battle game. With Erlang's fault tolerance and concurrency capabilities, this application is designed for reliability and scalability.

We hope this guide helps you set up and interact with the game. Feel free to contribute and expand the system as needed!

---