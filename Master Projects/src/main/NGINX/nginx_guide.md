# NGINX Load Balancer Configuration for Erlang Web Servers

## Overview
This configuration sets up NGINX as a WebSocket load balancer for three Erlang-based web servers. It ensures session persistence using consistent hashing and provides failover mechanisms for high availability.

## Configuration Breakdown

### General Settings
- **User & Processes**
    - `user www-data;` → Runs NGINX as a non-root user.
    - `worker_processes auto;` → Automatically adjusts worker processes based on CPU cores.

- **Network & Performance**
    - `worker_connections 768;` → Allows up to 768 connections per worker.
    - `sendfile on;` → Enables efficient file transfers.
    - `tcp_nopush on;` → Optimizes TCP packets.

- **Logging**
    - `access_log /var/log/nginx/access.log;` → Logs successful requests.
    - `error_log /var/log/nginx/error.log;` → Logs errors.

### Load Balancing & WebSocket Handling
- **Upstream Block (Backend Servers)**
    - `upstream erlang_servers { ... }` → Defines backend Erlang servers.
    - `hash $arg_GameID consistent;` → Ensures that the same client request is always routed to the same backend server.
    - `server 10.2.1.X:8080 max_fails=5 fail_timeout=30s;`
        - If a server fails **5 times** within **30 seconds**, it is temporarily removed from rotation.

- **WebSocket Proxy Settings**
    - `proxy_pass http://erlang_servers;` → Sends requests to the Erlang backend.
    - `proxy_http_version 1.1;` → Uses HTTP/1.1, necessary for WebSockets.
    - `proxy_set_header Upgrade $http_upgrade;` → Enables WebSocket connection upgrades.
    - `proxy_set_header Connection "Upgrade";` → Keeps WebSocket connections alive.
    - `proxy_set_header X-Real-IP $remote_addr;` → Passes client IP to the backend.

### Fault Detection & Failover
- **Error Handling**
    - `proxy_next_upstream error timeout http_500 http_502 http_503 http_504 invalid_header;`
        - If a request fails due to network errors, timeouts, or server-side failures (HTTP 500+), another backend is tried.

- **Retry Mechanism**
    - `proxy_next_upstream_timeout 1s;`
        - If a failure occurs, NGINX waits **1 second** before attempting a retry.
    - `proxy_next_upstream_tries 2;`
        - NGINX retries up to **2 times** before giving up, and try another node of the list.

### Conclusion
This configuration ensures efficient WebSocket handling, session persistence using hashing, and a robust failover mechanism. If a server experiences repeated failures, it is temporarily removed and retried after the specified timeout period.

---
