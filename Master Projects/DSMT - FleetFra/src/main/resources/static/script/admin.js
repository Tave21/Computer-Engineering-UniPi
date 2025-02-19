let admin_logged = sessionStorage.getItem("userLog"); // Get the logged-in player's username
// Button to logout
const logoutGameButton = document.querySelector("#logoutButton");

// Function that manages the logout of the user
$(document).ready(function () {
    logoutGameButton.onclick = function () {
        $.ajax({
            url: "http://10.2.1.26:5050/logout",
            type: "POST",
            data: admin_logged,
            dataType: "text",
            contentType: 'application/json',
            success: function () {
                // Remove the user from the session and go to the home page
                sessionStorage.removeItem("userLog");
                window.location.href = "index.html";
            },
            error: function (xhr) {
                alert(xhr.responseText);
            }
        });
    };
});

// Function to show and hide admin pages
function showPage(pageId) {
    document.querySelectorAll(".page").forEach(page => {
        page.style.display = page.id === pageId ? "block" : "none";
    });
}
// Function to search users
function searchUsers() {
    // Retrieve values written by the admin in the input fields
    const name = document.getElementById("search-name").value.toLowerCase();
    const surname = document.getElementById("search-surname").value.toLowerCase();
    const username = document.getElementById("search-username").value.toLowerCase();

    // Create the request to send to the Java server
    let requestAdmin = {};
    requestAdmin.firstName = name;
    requestAdmin.lastName = surname;
    requestAdmin.username = username;

    // Send the request to the java server
    $.ajax({
        url : "http://10.2.1.26:5050/viewDetailedUsers",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            const table = document.getElementById("users-table");
            // Create and fill the user table
            if (response && Array.isArray(response) && response.length > 0) {
                table.innerHTML = response
                    .filter(user => user.username !== admin_logged) // Remove the admin
                    .map(user => `
                        <tr>
                          <td>${user.firstName}</td>
                          <td>${user.lastName}</td>
                          <td>${user.username}</td>
                          <td>${user.email}</td>
                          <td>${user.playedGames}</td>
                          <td>${user.winGames}</td>
                          <td>${user.lostGames}</td>
                        </tr>`).join("");

                if ( document.getElementById("emptyUser").innerText.trim() !== "") {
                    // Remove the user not found message if not empty
                    document.getElementById("emptyUser").innerText = "";
                }

            }else{
                // Show the user not found message
                table.innerHTML = "";
                document.getElementById("emptyUser").innerText = "User not found";
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    })
}
// Function to search and remove user
function searchRemovableUsers() {
    // Retrieve values written by the admin in the input fields
    const name = document.getElementById("remove-name").value.toLowerCase();
    const surname = document.getElementById("remove-surname").value.toLowerCase();
    const username = document.getElementById("remove-username").value.toLowerCase();

    // Create the request to send to the Java server
    let requestAdmin = {};
    requestAdmin.firstName = name;
    requestAdmin.lastName = surname;
    requestAdmin.username = username;

    // Send the request to the java server
    $.ajax({
        url : "http://10.2.1.26:5050/viewUsers",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            const table = document.getElementById("remove-users-table");
            // Create and fill the user table, add also the remove button
            if (response && Array.isArray(response) && response.length > 0) {
                table.innerHTML = response
                    .filter(user => user.username !== admin_logged) // Remove the admin
                    .map(user => `
                        <tr id="user-${user.username}">
                          <td>${user.firstName}</td>
                          <td>${user.lastName}</td>
                          <td>${user.username}</td>
                          <td><button class="removeButton" onclick="removeUser('${user.username}')">Remove</button></td>
                        </tr>`).join("");

                if ( document.getElementById("removeUser").innerText.trim() !== "") {
                    // Remove the user not found message if not empty
                    document.getElementById("removeUser").innerText = "";
                }
            }else{
                // Show the user not found message
                table.innerHTML = "";
                document.getElementById("removeUser").innerText = "User not found";
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    })
}

// Function to remove user from the database
function removeUser(username) {
    // Send the request to the java server
    $.ajax({
        url: "http://10.2.1.26:5050/removeUser",
        data: username,
        dataType : "Text",
        type: "POST",
        contentType: 'application/json',
        success: function () {
            // Retrieve the correspondent row of the table
            const row = document.getElementById(`user-${username}`);
            if (row) {
                // Remove row from the table
                row.remove();
                console.log("User removed from table:", username);
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    });
}
// Function to show the matches of a time period
function filterMatches() {
    // Retrieve values selected by the admin in the data fields
    const startDate = new Date(document.getElementById("start-date").value);
    const endDate = new Date(document.getElementById("end-date").value);
    //Dates read are in a different format of the one in the database which contains data in the format 2024-12-02 17:00:00
    let requestAdmin = {};
    if ((!startDate || isNaN(startDate.getTime()))){
        // Send empty value
        requestAdmin.date1 = startDate;

    }else{
        // Change the date in the format used in the database
        requestAdmin.date1 = startDate.toISOString().split("T")[0];
    }
    if ((!endDate || isNaN(endDate.getTime()))) {
        requestAdmin.date2 = endDate;
    }else{
        requestAdmin.date2 = endDate.toISOString().split("T")[0]
    }
    // Send request to the Java server
    $.ajax({
        url : "http://10.2.1.26:5050/browseGamesAdmin",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            const table = document.getElementById("matches-table");
            // Create and fill the user table
            if (response && Array.isArray(response) && response.length > 0) {
                table.innerHTML = response.map(match => `
                <tr>
                  <td>${match.user1}</td>
                  <td>${match.user2}</td>
                  <td>${match.timestamp}</td>
                  <td>${match.winner}</td>
                </tr>`).join("");

                if ( document.getElementById("searchMatch").innerText.trim() !== "") {
                    // Remove the user not found message if not empty
                    document.getElementById("searchMatch").innerText = "";
                }

            }else{
                // Show the user not found message
                table.innerHTML = "";
                document.getElementById("searchMatch").innerText = "Match not found";
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    })

}
// Remove a placeholder in the input field if the user writes or clicks on it
// Add a placeholder in the input field if the user removes the inserted value or clicks out of it
document.addEventListener('DOMContentLoaded', () => {
    const inputs = document.querySelectorAll('input');
    inputs.forEach(input => {
        input.addEventListener('focus', () => {
            input.placeholder = '';
        });
        input.addEventListener('blur', () => {
            if (input.id === 'search-name') input.placeholder = 'Name';
            if (input.id === 'search-surname') input.placeholder = 'Surname';
            if (input.id === 'search-username') input.placeholder = 'Username';
            if (input.id === 'remove-name') input.placeholder = 'Name';
            if (input.id === 'remove-surname') input.placeholder = 'Surname';
            if (input.id === 'remove-username') input.placeholder = 'Username';
        });
    });
});