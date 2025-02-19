
---

# Game State Manager Documentation

## Overview

The `game_state_manager` module is an **Erlang GenServer** that manages game state storage using an **ETS table**. This module is responsible for storing, retrieving, and deleting game states. It also synchronizes game state changes across multiple nodes in a distributed Erlang system.

## Architecture

The module follows the **GenServer behavior**, which provides a framework for implementing server processes that handle synchronous (`handle_call`) and asynchronous (`handle_cast`) requests.

### Key Features:
- **Game State Storage:** Uses an ETS (Erlang Term Storage) table to store game states.
- **GenServer for Concurrency:** Ensures safe concurrent access and modification of the ETS table.
- **Distributed Synchronization:** Uses `rpc:call/4` to propagate game state changes to other nodes.
- **Automatic Cleanup:** Periodically removes expired game states.

---

## GenServer Behavior

A **GenServer** in Erlang is a process that handles messages in a structured way. It supports:
- **Synchronous requests** (`handle_call/3`)
- **Asynchronous messages** (`handle_cast/2`)
- **Info messages** (`handle_info/2`)

### Lifecycle of the GenServer:
1. **Initialization (`init/1`)**
    - Creates an **ETS table** to store game states.
    - Starts a **timer** for periodic cleanup.
    - Sets up the distributed environment with a node cookie.

2. **Handling Requests:**
    - **`handle_call/3`** (synchronous requests)
        - Used for operations that need an immediate response (e.g., storing, retrieving, or deleting a game state).
    - **`handle_cast/2`** (asynchronous requests)
        - Used for operations where an immediate response is not required (e.g., pushing to a queue).
    - **`handle_info/2`** (handling system messages)
        - Used for periodic cleanup of expired game states.

3. **Termination:**  
   The GenServer runs indefinitely unless stopped manually or due to a failure.

---

## ETS Synchronization (`rpc:cast/4`)

To keep the **ETS tables synchronized** across different Erlang nodes, the module uses **remote procedure calls (RPC)** with `rpc:call/4`.

### How `rpc:cast/4` Works
```erlang
rpc:cast(Node, Module, Function, Args)
```
- **`Node`**: The target remote node.
- **`Module`**: The module containing the function to execute.
- **`Function`**: The function to be called on the remote node.
- **`Args`**: A list of arguments to pass to the function.

This synchronizes the game state across all nodes in the cluster.

`rpc:cast/4` is a **fire-and-forget** operation that does not wait for a response.

---

## Distributed Synchronization Mechanism

To achieve **eventual consistency**, the module:
1. **Stores the game state locally in ETS**.
2. **Propagates updates to other nodes** using `rpc:cast/4`.
3. **Verifies synchronization** by checking the presence of the game state in remote nodes.

The synchronization process follows these steps:
- When a game state is added, updated, or deleted, the function `propagate_update/1` is called.
- It iterates over **all known nodes** in the cluster.
- If a node is **reachable**, it calls `rpc:cast/4` to execute `handle_sync_call/1` remotely.
- If a node is **unreachable**, it logs a message and continues.

This ensures that all nodes **eventually** will have the latest game state information.

---
