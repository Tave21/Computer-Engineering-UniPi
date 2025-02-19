let user_logged = sessionStorage.getItem("userLog"); // Get the logged-in player's username
let playerGrid // Matrix of the user grid
// Button to start the game and to logout
const startGameButton = document.querySelector("#startButton");
const logoutGameButton = document.querySelector("#homeButton");

// Function that manages the logout of the user, send the username to the Java server
$(document).ready(function () {
    logoutGameButton.onclick = function () {
        $.ajax({
            url: "http://10.2.1.26:5050/logout",
            type: "POST",
            data: user_logged,
            dataType: "text",
            contentType: 'application/json',
            success: function () {
                sessionStorage.removeItem("userLog");
                window.location.href = "index.html";
            },
            error: function (xhr) {
                alert(xhr.responseText);
            }
        });
    };
});
// Function that shows the waiting for the opponent window
$(document).ready(function () {
    startGameButton.onclick = function () {
        showWaitingScreen();
        document.getElementById("matchMaking").innerText = "Waiting for the opponent...";
        // Send request to find the opponent to the Java server
        sendStart().catch(error => {
            console.error("Error in sendStart:", error);
        });
    };
});
// Set the values for the user matrix
function setUserGrid(grid){
    playerGrid = grid;
}
// Function to send request to find the opponent to the Java server
async function sendStart(){

    $.ajax({
        url: "http://10.2.1.26:5050/game",
        data: user_logged,
        type: "POST",
        contentType: 'application/json',

        success: function (response) {
            let jsonResponse = JSON.parse(response);
            console.log("matchID:", jsonResponse.matchId);
            console.log("Player 1:", jsonResponse.player1);
            console.log("Player 2:", jsonResponse.player2);

            sessionStorage.setItem("gameId", jsonResponse.matchId);
            // Show the opponent username over the opponent grid
            document.getElementById("opponentHeader").innerText = jsonResponse.player2;
            // Show the opponent found message in the window
            document.getElementById("matchMaking").innerText = "Opponent found!";
            document.getElementById("matchMaking").style.color = "#07C043FF";

            // After 2 seconds, hide the waiting window
            setTimeout(function() {
                hideWaitingScreen();
            }, 2000);
            // Remove the style of the cells of the grid that are neighbour of the ships
            changeUserGrid();
            // Disable the start game button
            startGameButton.disabled = true;
            // Initialize the web socket for the game logic communication
            initializeWebSocket(jsonResponse.matchId, playerGrid);

        },
        error: function (xhr) {
            // Show an error message in the window
            document.getElementById("matchMaking").innerText = "Opponent not found!";
            document.getElementById("matchMaking").style.color = "#E70448E7";
            // After 2 seconds, hide the waiting window
            setTimeout(function() {
                hideWaitingScreen();
            }, 2000);
        }
    })
}
// Function to show the window of the match making
function showWaitingScreen() {
    // Creation of an overlay to disable the interaction with the page
    let overlay = document.createElement('div');
    overlay.id = "waitingOverlay";
    overlay.style.position = "fixed";
    overlay.style.top = "0";
    overlay.style.left = "0";
    overlay.style.width = "100%";
    overlay.style.height = "100%";
    overlay.style.backgroundColor = "rgba(0, 0, 0, 0.5)";
    overlay.style.display = "flex";
    overlay.style.justifyContent = "center";
    overlay.style.alignItems = "center";
    overlay.style.zIndex = "1000"; // It is over other elements

    // Creation of the waiting message
    let message = document.createElement('div');
    message.id = "matchMaking";
    message.style.backgroundColor = "white";
    message.style.padding = "20px";
    message.style.borderRadius = "10px";
    message.style.textAlign = "center";
    message.style.fontSize = "20px";

    // Add the message to the overlay
    overlay.appendChild(message);

    // Add the overlay to the body of the page
    document.body.appendChild(overlay);

    // Disable the interaction of the page
    document.body.style.pointerEvents = "none";
}

// Function to hide the waiting window
function hideWaitingScreen() {
    // Remove the overlay
    let overlay = document.getElementById("waitingOverlay");
    if (overlay) {
        document.body.removeChild(overlay);
    }

    // Enable the interaction with the page
    document.body.style.pointerEvents = "auto";
}

// Remove the class unavailable to the cells of the grid that are adjacent the ships
// The cells return with the original color
function changeUserGrid(){

    let boardName = "user";

        document.querySelectorAll(".cell").forEach((cell) => {

            // For the cells in the grid1
            if (cell.closest('#grid1')) {

                const [_, row, col] = cell.id.split(',').map(Number);

                if(playerGrid[row][col] === -1) {
                    let currentCell = document.getElementById(`${boardName},${row},${col}`);
                    currentCell.classList.remove("unavailable");
                }
            }
        });
}
// Function to enable or disable the opponent grid
function changeOpponentGrid(activate){

    let boardName = "opponent";

    document.querySelectorAll(".cell").forEach((cell) => {

        // For the cells in the grid2
        if (cell.closest('#grid2')) {

            const [_, row, col] = cell.id.split(',').map(Number);

            let currentCell = document.getElementById(`${boardName},${row},${col}`);
            // For the cells that contains number of letters the listeners aren't associated
            if(row !== 0 && col !== 0) {
                if(activate) {
                    // Activate the opponent's grid available cells
                    if((!currentCell.classList.contains("unavailable")) && (!currentCell.classList.contains("sink"))) {
                        currentCell.addEventListener("mouseover", changeCell);
                        currentCell.addEventListener("mouseout", restoreCell);
                        currentCell.addEventListener("click", shoot);
                    }
                }else{
                    // Deactivate the opponent's grid available cells
                    if (currentCell.classList.contains("eligible")){
                        currentCell.classList.remove("eligible");
                    }
                    currentCell.removeEventListener("mouseover", changeCell);
                    currentCell.removeEventListener("mouseout", restoreCell);
                    currentCell.removeEventListener("click", shoot);
                }
            }
        }
    });
}
// Function to add a style to a cell when the mouse is over it
function changeCell(e) {
    const [_, row, col] = e.target.id.split(',').map(Number); // Extract row and col of the cell

    document.getElementById(`opponent,${row},${col}`).classList.add("eligible");
}
// Function to remove a style to a cell when the mouse is out it
function restoreCell(e) {
    const [_, row, col] = e.target.id.split(',').map(Number); // Extract row and col of the cell

    document.getElementById(`opponent,${row},${col}`).classList.remove("eligible");
}
// Function to shot
function shoot(e) {
    const [_, row, col] = e.target.id.split(',').map(Number); // Extract row and col of the cell
    console.log("shot "+row+" - "+col);
    // Send the hit cell to the Erlang server
    sendMoveMessage(row, col);
}
