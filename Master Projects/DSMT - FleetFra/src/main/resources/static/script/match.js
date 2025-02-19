let socket; // WebSocket connection instance
let player_username = sessionStorage.getItem("userLog"); // Get the logged-in player's username
let game_id; // Stores the current game ID
let player_battlefield; // Stores the player's battlefield grid
let timerInterval; // Timer for turn countdown
let intervalId = null; // Variable to store the timer ID
let row, col; // Stores the last move coordinates
let last_update = null; // Stores the timestamp of the last game update
let turn; // Boolean flag indicating if it's the player's turn
let user1, user2, game_winner; // Store player names and winner
let start = false; // Variable to let the waiting player timer start
let error_start = false;

// Function to initialize the WebSocket
function initializeWebSocket(current_match, current_battlefield) {
    // Retrieve the game id and the player battlefield only at the start of the match
    if(!error_start) {
        game_id = current_match;
        player_battlefield = current_battlefield;
    }
    // Create a web socket connected to the load balancer with the game id as attribute
    const serverAddress = `ws://10.2.1.27:8080/ws?GameID=${game_id}`;
    socket = new WebSocket(serverAddress);

    socket.addEventListener("open", (event) => {
        console.log("WebSocket connection established:", event);
        //When the websocket is created, the startMessage of the player is sent to the server
        if(!error_start) {
            sendStartMessage();
        }

    });

    // Server sent a message
    socket.addEventListener("message", (event) => {
        handleServerMessage(event.data);

    });

    // Socket closed
    socket.addEventListener("close", (event) => {
        console.log("WebSocket connection closed:", event);
        //Reopen the socket if it is closed during a match
        error_start = true;
        setTimeout(initializeWebSocket, 2000);

    });


    // Error during the connection
    socket.addEventListener("error", (event) => {
        console.error("WebSocket error:", event);
        //Reopen the socket if it is closed during a match
        error_start = true;
        setTimeout(initializeWebSocket, 2000);

    });
}

// Function to send the start message to the Erlang server
function sendStartMessage() {
    if (socket && socket.readyState === WebSocket.OPEN) {
        // Create the json to send the battlefield of the player to the server
        let message = createBattlefieldJson(player_battlefield);

        setTimeout(function() {
            socket.send(message);
        }, 1000);

        console.log("Start message sent:", message);
    } else {
        console.error("WebSocket is not open.");
    }
}
// Function to send the move message to the Erlang server
function sendMoveMessage(current_row, current_col) {
    // Deactivate the grid when the user send the coordinates to avoid send a double invalid move
    changeOpponentGrid(false);
    row = current_row;
    col = current_col;

    if (socket && socket.readyState === WebSocket.OPEN) {
        // Create the json to send the coordinates clicked by the player to the server
        let message = createMoveJson(row, col);

        setTimeout(function() {
            socket.send(message);
        }, 1000);

        console.log("Move message sent:", message);
    } else {
        console.error("WebSocket is not open.");
    }
}

// Function to send the get game message to the Erlang server
function sendGetGameMessage(){

    if (socket && socket.readyState === WebSocket.OPEN) {
        // Creation of the Json message to send to the Erlang server
        const getGameData = {
            type_request: "get_game_info",
            game_id: game_id
        };

       let message = JSON.stringify(getGameData, null, 2);

        setTimeout(function() {
            socket.send(message);
        }, 1000);

        console.log("Get game info message sent:", message);

    } else {
        console.log("WebSocket is not open.");

    }
}

// Function to send the change turn message to the Erlang server
function sendChangeTurnMessage(){

    if (socket && socket.readyState === WebSocket.OPEN) {
        // Creation of the Json message to send to the Erlang server
        const sendChangeData = {
            type_request: "change_turn",
            game_id: game_id
        };

        let message = JSON.stringify(sendChangeData, null, 2);

        setTimeout(function() {
            socket.send(message);
        }, 1000);

        console.log("Change turn message sent:", message);
    } else {
        console.error("WebSocket is not open.");
    }
}

// Function to handle messages received from the server
function handleServerMessage(serverMessage) {

    try {
        const message = JSON.parse(serverMessage);
        //If the message hasn't a field "message", it a game update
        if (!message.message) {
            console.log("GET GAME jSON");
            // Extract data from the message received
            let extractedData = extractGameData(message);
            // Initialize the value of the last update
            if(last_update == null){
                last_update = extractedData.created_at;
            }
            // Check if the game is finished
            if(extractedData.winner !== "none"){

                if(extractedData.winner === player_username){
                    // Only for the winner player, the match is saved in the database
                    // the match must be inserted only once for the two players of a match
                    insertMatch();
                    // Reload page and close the connection when the game ends
                    setTimeout(function() {
                        hideWaitingScreen();
                        reloadPage();
                    }, 2000);


                }else{
                    //If the player is not the winner, show the lost message, reload page and close the connection
                    showWaitingScreen();
                    document.getElementById("matchMaking").innerText = "YOU LOST!";
                    document.getElementById("matchMaking").style.color = "#E70448E7";
                    // After 2 seconds, hide the waiting window
                    setTimeout(function() {
                        hideWaitingScreen();
                        reloadPage();
                    }, 2000);
                }
            } else if(player_username === extractedData.current_turn){
                // It's current player turn
                turn = true;
                // Stop the periodic request if the player has the turn
                stopPeriodicExecution();
                // Activate opponent grid
                changeOpponentGrid(true);
                if(!start){
                    // Start is always true for the player that starts the game
                    start = true;
                }
                // Update grid of the player
                updatePlayerGrid(extractedData.battlefieldMatrix);
                // Reset and start the timer
                resetTimer();
                startTimer();

            }else if(player_username === extractedData.waiting_player){
                // It's not current player turn
                turn = false;
                // Deactivate opponent grid
                changeOpponentGrid(false);
                if(!start){
                    // Reset and start the timer only the first time the player is waiting
                    resetTimer();
                    startTimer();
                    start = true;
                }
                // Start periodic requests to the server
                startPeriodicExecution();

                if(last_update !== extractedData.created_at){
                    // There is a new update, so there is a new turn of the same active player
                    console.log("last update is different");
                    // Update grid of the player
                    updatePlayerGrid(extractedData.battlefieldMatrix);
                    // Reset and start the timer
                    resetTimer();
                    startTimer();
                    // Update the last update value
                    last_update = extractedData.created_at;
                }
            }
            // Set the field of the player turn
            setPlayerTurn(extractedData.current_turn);

        }else {

            const responseMessage = message.message;
            // Case-insensitive check for error messages
            if ((/error/i.test(responseMessage) || /invalid/i.test(responseMessage)) && (responseMessage !== "TURN ERROR: Not your turn") ) {
                // Only the turn error is excluded, in this case, the turn of the current player ends and starts the turn of other player
                // View the error message
                showWaitingScreen();
                document.getElementById("matchMaking").innerText = "Error: " + responseMessage;
                document.getElementById("matchMaking").style.color = "#E70448E7";
                // After 2 seconds, hide the waiting window, close the connection and reload the page
                setTimeout(function() {
                    hideWaitingScreen();
                    reloadPage();
                }, 2000);

            }

            switch (responseMessage) {
                case "OK: Game started":
                    console.log("OK: Game started");
                    // If the game has started, the two player request info to know who start
                    sendGetGameMessage();
                    break;
                case "OK: Move accepted [2]":
                    console.log("Move accepted - ship");
                    //Update only the opponent grid and ask info to see if the player has won
                    updateOpponentCell(true);
                    sendGetGameMessage();
                    break;
                case "OK: Move accepted [3]":
                    console.log("Move accepted - water");
                    //Update only the opponent grid and ask the update to suspend
                    updateOpponentCell(false);
                    sendGetGameMessage();
                    break;
                case "OK: Turn changed":
                    console.log("OK: Turn changed");
                    // The timer has elapsed, so ask the update
                    sendGetGameMessage();
                    break;
                case "VICTORY":
                    console.log("Congratulations! You won!");
                    // Show the win message
                    showWaitingScreen();
                    document.getElementById("matchMaking").innerText = "YOU WIN!";
                    document.getElementById("matchMaking").style.color = "#07C043FF";
                    sendGetGameMessage(); // Necessary otherwise the field winner is none
                    break;
                default:
                    console.log("Unknown message");
            }
        }
    } catch (error) {
        console.error("Error parsing JSON message:", error);
    }

}
// Function to insert the match in the database
function insertMatch(){
    // Current date
    let timestamp =  new Date();
    // Add 1 hour, because the hour read is 1 hour late
    timestamp.setHours(timestamp.getHours() + 1);
    // Format the date as YYYY-MM-DD HH:MM:SS
    let formattedDate = timestamp.toISOString().replace('T', ' ').substring(0, 19);

    let win;
    // If the user1 is the game winner, assign 1, otherwise 0
    if (user1 === game_winner){
        win = "1";
    }else{
        win = "0";
    }
    // Create the message to send to the Java Server
    let match = {
        user1: user1,
        user2: user2,
        id: 1,
        winner: win,
        timestamp: formattedDate
    };
    // Send the data to the Java server
    $.ajax({
        url : "http://10.2.1.26:5050/insertMatch",
        data : JSON.stringify(match),
        type : "POST",
        dataType: "text",
        contentType: 'application/json',
        success: function (data) {
            console.log(data);
        },
        error: function(xhr) {
            console.log(xhr);
        }
    })
}
// Update the color of the clicked opponent cell
function updateOpponentCell(sink){
    let boardName = "opponent";

    let cell = document.getElementById(`${boardName},${row},${col}`);
    // If there is a ship in the cell, assign the red color, otherwise the water color
    if(sink){
        cell.classList.add("sink");
    }else{
        cell.classList.add("unavailable");
    }
}
// Update the color of the clicked user cell
function updatePlayerGrid(grid){
    let boardName = "user";

    document.querySelectorAll(".cell").forEach((cell) => {

        // For all the cells in the grid1
        if (cell.closest('#grid1')) {

            const [_, row, col] = cell.id.split(',').map(Number);
            let currentCell = document.getElementById(`${boardName},${row},${col}`);
            // If there is a ship in the cell, assign the red color, otherwise the water color
            if(grid[row][col] === 2) {
                currentCell.classList.add("sink");
            }else if(grid[row][col] === 3){
                currentCell.classList.add("unavailable");
            }
        }
    });
}
// Extract data from the update message
function extractGameData(gameData) {

    const { created_at, current_turn, player1, player2, waiting_player, winner, battlefields } = gameData;
    // Assign the values necessary for the insertion of the match
    user1 = player1;
    user2 = player2;
    game_winner = winner;
    // Retrieve the battlefield
    const battlefield = battlefields[player_username];
    if (!battlefield) {
        console.error("No data found for the player");
        return [];
    }
    // Assign the value of the updated battlefield to the matrix
    let maxRow = 0;
    let maxCol = 0;

    battlefield.forEach(cell => {
        if (cell.row > maxRow) maxRow = cell.row;
        if (cell.col > maxCol) maxCol = cell.col;
    });

    const battlefieldMatrix = Array.from({ length: maxRow + 1 }, () =>
        Array(maxCol + 1).fill(0)
    );

    battlefield.forEach(cell => {
        battlefieldMatrix[cell.row][cell.col] = cell.value;
    });

    return {
        created_at,
        current_turn,
        waiting_player,
        winner,
        battlefieldMatrix
    };
}

// Close the websocket connection and reload the page
function reloadPage() {
    socket.close();
    console.log("Closing WebSocket connection.");
    window.location.reload();
}
// Create the Json that represents the player's move
function createMoveJson(row, col) {
    const moveData = {
        move: { col: col, row: row },
        type_request: "make_move",
        player: player_username,
        game_id: game_id
    };

    return JSON.stringify(moveData, null, 2);
}

// Create a JSON with the field "player_battlefield" from a matrix
function createBattlefieldJson(matrix) {
    let battlefield = [];

    for (let row = 0; row < matrix.length; row++) {
        for (let col = 0; col < matrix[row].length; col++) {
            battlefield.push({
                row: row,
                col: col,
                value: matrix[row][col]
            });
        }
    }

    const battlefieldData = {
        game_id: game_id,
        player: player_username,
        type_request: "start_game_client",
        player_battlefield: battlefield

    };

    return JSON.stringify(battlefieldData, null, 2);
}
// Function to start the timer
function startTimer() {
    let timeLeftElement = document.getElementById("timeLeft");
    let timeLeft = parseInt(timeLeftElement.textContent);

    if (timerInterval) clearInterval(timerInterval); // Prevent from double executions

    timerInterval = setInterval(() => {
        if (timeLeft > 0) {
            timeLeft--;
            timeLeftElement.textContent = timeLeft;
        } else {
            clearInterval(timerInterval);
            console.log("Timer elapsed!");
            // only who has the turn, if the timer elapsed, send the request
            if(turn){
                sendChangeTurnMessage();
            }
        }
    }, 1000);
}
// Function to reset the timer
function resetTimer() {
    clearInterval(timerInterval);
    document.getElementById("timeLeft").textContent = "15";
}
// Function to write, on the page, the username of the player that has the turn
function setPlayerTurn(player) {
    // Change color if the player is waiting or if is playing
    if(!turn){
        document.getElementById("playerTurn").style.color = "#ff8000";
    }else{
        document.getElementById("playerTurn").style.color = "#256ec9";
    }
    document.getElementById("playerTurn").textContent = player +" turn";
}


// Function to start the periodic call of sendGetGameMessage
function startPeriodicExecution() {
    if (intervalId == null) { // Avoid to start more timer
        intervalId = setInterval(sendGetGameMessage, 1000);
        console.log("Periodic execution started");
    }
}

// Function to stop the periodic call of sendGetGameMessage
function stopPeriodicExecution() {
    if (intervalId != null) {
        clearInterval(intervalId);
        console.log("Periodic execution ended");
        intervalId = null;
    }
}