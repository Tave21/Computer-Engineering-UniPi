// Function to manage the view of the login and signup forms
function toggleForms() {
    const loginForm = document.getElementById('login-form');
    const signupForm = document.getElementById('signup-form');
    if (loginForm.style.display === 'none') {
        loginForm.style.display = 'block';
        signupForm.style.display = 'none';
    } else {
        loginForm.style.display = 'none';
        signupForm.style.display = 'block';
    }
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
            if (input.id === 'login-username') input.placeholder = 'Enter your username';
            if (input.id === 'login-password') input.placeholder = 'Enter your password';
            if (input.id === 'signup-name') input.placeholder = 'Enter your name';
            if (input.id === 'signup-surname') input.placeholder = 'Enter your surname';
            if (input.id === 'signup-username') input.placeholder = 'Choose a username';
            if (input.id === 'signup-email') input.placeholder = 'Enter your email';
            if (input.id === 'signup-password') input.placeholder = 'Create a password';
        });
    });
});

$(document).ready(function () {
    document.querySelector('button[class="login"]').addEventListener('click', (e) => {
        e.preventDefault();
        let isEmpty = false;

        // Reset all error messages
        document.querySelectorAll('.error-message').forEach(msg => msg.style.display = 'none');

        // Username validation
        const username = document.getElementById('login-username').value.trim();
        if (username.length === 0) {
            document.getElementById('error-login-username').style.display = 'block';
            isEmpty = true;
        }
        // Password validation
        const password = document.getElementById('login-password').value.trim();
        if (password.length === 0) {
            document.getElementById('error-login-password').style.display = 'block';
            isEmpty = true;
        }
        // If no errors, submit the form
        if (!isEmpty) {
            // Create the message to send to the Java server
            let requestUser = {
                username: username,
                password: password
            };
            // Send the message to the Java server
            $.ajax({
                url: "http://10.2.1.26:5050/login",
                data: JSON.stringify(requestUser),
                type: "POST",
                dataType: "json",
                contentType: 'application/json',
                success: function () {
                    // If the user is not an admin add teh username to the userLog session and go to the game page
                    if (username === "admin") {
                        sessionStorage.setItem("userLog", "admin");
                        window.location.href = "admin.html";

                    } else {
                        sessionStorage.setItem("userLog", username);
                        window.location.href = "game.html";
                    }

                },
                error: function (xhr) {
                    let errorResponse = JSON.parse(xhr.responseText);
                    // Show the error message if the password is wrong
                    document.getElementById("error-login-password").innerText = errorResponse.answer;
                    document.getElementById('error-login-password').style.display = 'block';

                }
            })

        }
    });
});

$(document).ready(function () {
    document.querySelector('button[class="signup"]').addEventListener('click', (e) => {
        e.preventDefault();
        let isEmpty = false;

        // Reset all error messages
        document.querySelectorAll('.error-message').forEach(msg => msg.style.display = 'none');

        // Name validation
        const name = document.getElementById('signup-name').value.trim();
        if (name.length < 3 || /[^a-zA-Z]/.test(name)) {
            document.getElementById('error-signup-name').style.display = 'block';
            isEmpty = true;
        }

        // Surname validation
        const surname = document.getElementById('signup-surname').value.trim();
        if (surname.length < 3 || /[^a-zA-Z]/.test(surname)) {
            document.getElementById('error-signup-surname').style.display = 'block';
            isEmpty = true;
        }

        // Username validation
        const username = document.getElementById('signup-username').value.trim();
        if (username.length < 5) {
            document.getElementById('error-signup-username').style.display = 'block';
            isEmpty = true;
        }

        // Email validation
        const email = document.getElementById('signup-email').value.trim();
        const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
        if (!emailRegex.test(email)) {
            document.getElementById('error-signup-email').style.display = 'block';
            isEmpty = true;
        }

        // Password validation
        const password = document.getElementById('signup-password').value.trim();
        if (password.length < 5) {
            document.getElementById('error-signup-password').style.display = 'block';
            isEmpty = true;
        }

        // If no errors, submit the form (this is where you can handle the submission logic)
        if (!isEmpty) {

            // only a normal user can register, an admin is already registered
            let person = {
                firstName: name,
                lastName: surname,
                username: username,
                password: password,
                email: email
            };
            // Send the message to the Java server
            $.ajax({
                url: "http://10.2.1.26:5050/signup",
                data: JSON.stringify(person),
                type: "POST",
                contentType: 'application/json',

                success: function () {
                    sessionStorage.setItem("userLog", username);
                    window.location.href = "game.html";
                },
                error: function (xhr) {
                    // Show the error message if the username already exists
                    document.getElementById('error-signup-username').innerText = xhr.responseText;
                    document.getElementById('error-signup-username').style.display = 'block';
                }
            })
        }
    });
});
