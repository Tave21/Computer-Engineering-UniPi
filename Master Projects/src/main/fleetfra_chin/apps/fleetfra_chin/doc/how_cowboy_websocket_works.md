Cowboy uses WebSockets in a highly efficient and lightweight way by leveraging its **event-driven architecture**.

### How Cowboy Uses WebSocket

1. **WebSocket Handshake:**
    - When a client (usually a browser) requests a WebSocket connection, it first sends an HTTP request to the server (this is called the WebSocket handshake).
    - Cowboy, as the web server, processes this handshake by upgrading the HTTP connection to a WebSocket connection using the `Sec-WebSocket` protocol headers.

2. **Persistent Connection:**
    - Once the handshake is successful, the connection is "upgraded" from HTTP to WebSocket. This means that the TCP connection remains open for the duration of the session, allowing for bi-directional communication.
    - WebSocket messages can now be exchanged between the client and the server without needing to re-establish the connection for each message.

### Process Creation and Flow in Cowboy

Cowboy follows an **event-driven, process-per-connection model**, which is quite different from the traditional **thread-per-request** model found in many other web servers. Here's how it works:

1. **Initial Request Handling:**
    - When a WebSocket request (or any HTTP request) comes into Cowboy, a new Erlang process is created to handle that specific request.
    - Cowboy processes the request in an isolated Erlang process, so each connection (including WebSocket) has its own dedicated process. This allows for concurrency and parallelism without blocking the main thread or other connections.

2. **Handling WebSocket Connections:**
    - After the WebSocket handshake, Cowboy creates a new Erlang process for that WebSocket connection. This process is responsible for managing the ongoing communication between the client and server.
    - Every message sent over the WebSocket connection is handled by this Erlang process. This means that each WebSocket connection (per client) operates in its own lightweight Erlang process.
    - Cowboy uses the **gen_server** behavior for managing stateful processes. Each WebSocket connection might be tied to a gen_server that keeps track of the state for that particular client.

3. **Efficient Message Handling:**
    - Cowboy’s design ensures that each connection operates in a separate lightweight Erlang process. This approach avoids the overhead of managing threads (as in traditional web servers) and takes advantage of Erlang's **actor model**.
    - Cowboy also uses **event loops** to handle incoming WebSocket messages. These event loops are very efficient in handling multiple messages concurrently by switching between processes as needed.

4. **Scaling and Concurrency:**
    - Cowboy's lightweight process model allows it to handle thousands (or even millions) of WebSocket connections concurrently, as each WebSocket connection gets its own process and there’s no blocking.
    - Erlang processes are very lightweight, which means that Cowboy can scale horizontally by adding more nodes (Erlang nodes) to the cluster. Each node in the cluster can handle many concurrent WebSocket connections.

### What Happens When Cowboy Receives a WebSocket Request?

- **Step 1: WebSocket Handshake**
    - A client sends a WebSocket upgrade request (HTTP request) to Cowboy.
    - Cowboy receives the request and creates an Erlang process to handle this request.
    - If the handshake is successful, the connection is upgraded to WebSocket.

- **Step 2: Maintain Persistent Connection**
    - Cowboy creates a new Erlang process dedicated to this WebSocket connection. This process listens for messages from the client and responds to them.
    - The connection remains open, and the process continues to listen for incoming messages until the WebSocket is closed or terminated.

- **Step 3: Message Exchange**
    - Once the WebSocket connection is established, messages can be sent from the client to the server and vice versa.
    - The Erlang process created for that connection handles the incoming WebSocket messages and forwards them to the appropriate handler, often a gen_server that maintains the game state or some other process.

- **Step 4: Closing the Connection**
    - When the WebSocket is closed (either by the client or server), the Erlang process is terminated, freeing up resources.

### Summary of the Process Flow

- Cowboy receives the WebSocket upgrade request via HTTP.
- It creates an Erlang process to handle the request.
- After the handshake, Cowboy establishes a WebSocket connection and assigns a new Erlang process to handle the messages on that connection.
- Messages are exchanged between the client and the server through this Erlang process.
- When the connection is closed, the process is terminated.

### Key Benefits of Cowboy’s Approach

- **Low Overhead:** Since each connection is handled by its own lightweight Erlang process, there’s no need for heavy thread management, and the system can efficiently handle a large number of connections.
- **Concurrency:** The Erlang runtime efficiently manages multiple processes, allowing for high concurrency without the risk of blocking.
- **Fault-Tolerance:** If a process handling a WebSocket connection crashes, it can be restarted without affecting other connections, ensuring the system is highly resilient.

In essence, Cowboy's use of Erlang processes and WebSockets allows it to handle many simultaneous WebSocket connections efficiently and reliably, with each connection being handled by a dedicated, isolated process.