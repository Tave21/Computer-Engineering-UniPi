let user = sessionStorage.getItem("userLog"); // Get the logged-in player's username
// Matrix that contains values of the user battlefield
const userGrid = Array(11).fill(0).map(() => Array(11).fill(0));
// Button to rotate the fleet and button to start the game
const rotateButton = document.querySelector("#rotateButton");
const startButton = document.querySelector("#startButton");
// Variable to save the state of orientation of the fleet (horizontal or vertical)
let isHorizontal = true;

// Definition of the available ships with name, dimension and unique index
const shipArray = [
    { name: "single", size: 1, index: 0 },
    { name: "single", size: 1, index: 1 },
    { name: "single", size: 1, index: 2 },
    { name: "single", size: 1, index: 3 },
    { name: "double", size: 2, index: 4 },
    { name: "double", size: 2, index: 5 },
    { name: "double", size: 2, index: 6 },
    { name: "triple", size: 3, index: 7 },
    { name: "triple", size: 3, index: 8 },
    { name: "quadruple", size: 4, index: 9 }
];

// The button allows to change the orientation of the fleet
rotateButton.addEventListener("click", rotateShips);

// Function to change the orientation of the fleet
function rotateShips() {
    const fleetItems = document.querySelectorAll('.fleet_item');
    fleetItems.forEach((item) => {
        const classes = item.classList;
        //If the fleet is horizontal, change to vertical, otherwise apply the opposite operation
        if (isHorizontal) {
            classes.add(`${item.classList[1]}-vertical`);
            classes.remove(`${item.classList[1]}`);
        } else {
            classes.add(`${item.classList[1].replace('-vertical', '')}`);
            classes.remove(`${item.classList[1]}`);
        }
    });
    isHorizontal = !isHorizontal; // Invert the state of the orientation
}

// Function to create the game grid
function createGrid(gridId) {
    const grid = document.getElementById(gridId);
    const letters = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"];

    for (let i = 0; i <= 10; i++) {
        for (let j = 0; j <= 10; j++) {
            const cell = document.createElement("div"); //Create cell
            cell.classList.add("cell");

            if (i === 0 && j === 0) {
                cell.classList.add("cell-header"); //Cell header of the top-left corner
            } else if (i === 0) {
                cell.textContent = letters[j - 1];
                cell.classList.add("cell-header"); //Row cell header
            } else if (j === 0) {
                cell.textContent = i.toString();
                cell.classList.add("cell-header"); //Col cell header
            }
            cell.id = `${gridId === "grid1" ? "user" : "opponent"},${i},${j}`; // Add id to the cell
            grid.appendChild(cell); // Add the cell to the grid
        }
    }
}
// Handle mouse hover the grid for positioning the ships
function handleDivMouseOver(e) {
    const [_, row, col] = e.target.id.split(',').map(Number); // Extract row and col from the cell
    let isValid = true;

    for (let i = 0; i < e.target.shipSize; i++) {
        const targetRow = isHorizontal ? row : row + i; // Compute the position of the target cells
        const targetCol = isHorizontal ? col + i : col;

        // Verification if the cells are valid for the positioning
        if (targetRow > 10 || targetCol > 10 || userGrid[targetRow][targetCol] !== 0) {
            isValid = false;
            break;
        }
    }

    if (isValid) {
        // Highlight the valid cells
        for (let i = 0; i < e.target.shipSize; i++) {
            const targetRow = isHorizontal ? row : row + i;
            const targetCol = isHorizontal ? col + i : col;
            document.getElementById(`user,${targetRow},${targetCol}`).classList.add("eligible");
        }
    }
}

// Function to manage the mouseout, by removing the highlight while positioning the ship
function handleDivMouseOut(e) {
    const [_, row, col] = e.target.id.split(',').map(Number); // Extract row and col from the cell
    // Remove the highlight the valid cells
    for (let i = 0; i < e.target.shipSize; i++) {
        const targetRow = isHorizontal ? row : row + i;
        const targetCol = isHorizontal ? col + i : col;
        if (targetRow <= 10 && targetCol <= 10) {
            document.getElementById(`user,${targetRow},${targetCol}`).classList.remove("eligible");
        }
    }
}

// Function to manage the click on the cells and place the ship
function handleDivClick(e) {
    if (e.target.classList.contains("eligible")) {
        const [_, row, col] = e.target.id.split(',').map(Number); // Extract row and col from the cell

        const ship = {
            name: shipArray[e.target.ship.id].name, // Name of the ship
            row,
            column: col,
            size: e.target.shipSize,
            direction: isHorizontal ? 1 : 0, // Orientation: 1 for horizontal, 0 for vertical
        };

        placeShip(ship, "user"); // Place the ship in the user grid
        //removing the ship from the listener so that it can't be reinserted once inserted
        for (let row = 1; row <= 10; row ++) {
            for (let col = 1; col <= 10; col ++) {
                let trash = document.getElementById("user,"+row+","+col);
                trash.removeEventListener("mouseover", handleDivMouseOver);
                trash.removeEventListener("mouseout", handleDivMouseOut);
                trash.removeEventListener("click", handleDivClick);
            }
        }

        const fleetItem = document.getElementById(e.target.ship.id); // Remove the ship from the Fleet list
        fleetItem.remove();
        // When all the ships are positioned, the user can start the game by clicking on the button
        if(isFleetEmpty()){
            rotateButton.disabled = true;
            startButton.disabled = false;

            userGrid.forEach(row => {
                console.log(row.join(' '));
            });
            // Set the user grid in the game.js page
            setUserGrid(userGrid);

        }
    }
}
// Check if all ships are placed into the grid
function isFleetEmpty() {
    const fleet = document.getElementById("fleet");
    const fleetItems = fleet.getElementsByClassName("fleet_item");
    console.log("length: "+fleetItems.length);
    return fleetItems.length === 0;
}
// Function to place the ship in the grid
function placeShip(ship, boardName) {
    const deltas = [-1, 0, 1];

    for (let i = 0; i < ship.size; i++) {
        // For each cell in which the ship is positioned into, change the style to insert the ship
        const targetRow = ship.direction === 1 ? ship.row : ship.row + i;
        const targetCol = ship.direction === 1 ? ship.column + i : ship.column;

        userGrid[targetRow][targetCol] = 1; // The cell is not free
        const cell = document.getElementById(`${boardName},${targetRow},${targetCol}`);
        cell.classList.remove("eligible"); // Remove eligible and assign taken
        cell.classList.add("taken", ship.name);
    }

    // Assign the class "unavailable" at all the neighbour cells of the ship
    for (let i = 0; i < ship.size; i++) {
        const row = ship.direction === 1 ? ship.row : ship.row + i;
        const col = ship.direction === 1 ? ship.column + i : ship.column;

        deltas.forEach((dx) => {
            deltas.forEach((dy) => {
                const adjacentRow = row + dx;
                const adjacentCol = col + dy;
                // Check if the neighbour cells are not out of the grid
                if (
                    adjacentRow > 0 &&
                    adjacentRow <= 10 &&
                    adjacentCol > 0 &&
                    adjacentCol <= 10 &&
                    userGrid[adjacentRow][adjacentCol] === 0
                ) {
                    userGrid[adjacentRow][adjacentCol] = -1; // Sign the cell as not available (neighbour of the ship)
                    const adjacentCell = document.getElementById(`${boardName},${adjacentRow},${adjacentCol}`);
                    if (adjacentCell) {
                        adjacentCell.classList.add("unavailable");
                    }
                }
            });
        });
    }
}

// Initialization of the page
window.onload = function () {

    document.getElementById("playerHeader").innerText = user;

    createGrid("grid1"); // Create the user grid
    createGrid("grid2"); // Create the opponent grid
    startButton.disabled = true;

    const fleetItems = document.querySelectorAll('.fleet_item');
    fleetItems.forEach((item) => {
        // Add mouseover, mouseout and click events for each cell
        item.addEventListener("click", () => {
            document.querySelectorAll(".cell").forEach((cell) => {
                // Check that the cell is in the grid1
                if (cell.closest('#grid1')) {
                    const [_, row, col] = cell.id.split(',').map(Number);
                    // For the cells that contains number of letters the listeners aren't associated
                    if(row !== 0 && col !== 0) {
                        cell.ship = item;
                        cell.shipSize = shipArray[item.id].size;
                        cell.addEventListener("mouseover", handleDivMouseOver);
                        cell.addEventListener("mouseout", handleDivMouseOut);
                        cell.addEventListener("click", handleDivClick);
                    }
                }
            });
        });
    });
};
