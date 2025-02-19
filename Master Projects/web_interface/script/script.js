/**
 * Creates and displays a network graph from a JSON file.
 * The graph is rendered using the Vis.js library.
 *
 * @param {string} jsonFilePath - The path to the JSON file containing network data.
 *
 * JSON file structure:
 * {
 *   "nodes": [
 *     { "id": "1", "label": "Node 1", "image": "node1.png" },
 *     { "id": "2", "label": "Node 2", "image": "node2.png" }
 *   ],
 *   "links": [
 *     { "from": "1", "to": "2", "color": { "color": "red" } }
 *   ]
 * }
 */
async function createNetworkFromJson(jsonFilePath) {
    try {
        // Fetch the JSON file and parse its content.
        const response = await fetch(jsonFilePath);
        const data = await response.json();

        // Map nodes to Vis.js format.
        const nodes = data.nodes.map(node => ({
            id: node.id, // Unique identifier for the node.
            label: node.label, // Label to display for the node.
            image: `images/${node.image}`, // Path to the node's image.
            shape: 'image' // Use the image shape for nodes.
        }));

        // Map links (edges) to Vis.js format.
        const edges = data.links.map(link => ({
            from: link.from, // Starting node ID of the edge.
            to: link.to, // Ending node ID of the edge.
            color: link.color || { color: 'lightgreen' } // Edge color (default is light green).
        }));

        // Select the container where the network graph will be displayed.
        const container = document.getElementById('container');

        // Prepare the data for the network graph.
        const networkData = {
            nodes: new vis.DataSet(nodes), // Create a DataSet for nodes.
            edges: new vis.DataSet(edges) // Create a DataSet for edges.
        };

        // Configure the graph's appearance and behavior.
        const options = {
            layout: { hierarchical: false }, // Disable hierarchical layout.
            interaction: { hover: true }, // Enable hover interactions.
            nodes: { shape: 'image' }, // Set the default node shape to image.
            edges: { width: 3 } // Set the default edge width.
        };

        // Create and render the network graph using Vis.js.
        new vis.Network(container, networkData, options);

    } catch (error) {
        // Handle errors (e.g., file not found, invalid JSON structure).
        console.error("Error creating network from JSON:", error.message);
    }
}

/**
 * Updates the displayed file name when a file is selected in an input element.
 * @param {Event} event - The change event triggered by the file input.
 */
function updateFileName(event) {
    const inputElement = event.target; // File input element that triggered the event.
    const fileNameSpanId = inputElement.id.replace('file-input', 'file-name'); // Derive the <span> ID to update.
    const fileNameSpan = document.getElementById(fileNameSpanId);

    if (inputElement.files.length) {
        fileNameSpan.textContent = inputElement.files[0].name; // Display the selected file's name.
    } else {
        fileNameSpan.textContent = 'No file selected'; // Fallback message if no file is selected.
    }
}

/**
 * Uploads a network file to the server.
 * Validates file selection, sends the file using a POST request, and handles the server response.
 */
async function uploadNetworkFile() {
    const fileInput = document.getElementById('file-input');
    const resultSpan = document.getElementById('upload-result');
    resultSpan.textContent = '';

    // Validate that a file is selected.
    if (!fileInput.files.length) {
        resultSpan.textContent = 'Please select a file to upload.';
        resultSpan.className = 'result-message error';
        return;
    } else {
        resultSpan.textContent = "Cooking";
    }

    const file = fileInput.files[0];
    const formData = new FormData();
    formData.append('networkFile', file);

    try {
        // Send the file to the server via POST request.
        const response = await fetch('http://127.0.0.1:5000/upload', {
            method: 'POST',
            body: formData
        });

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        const result = await response.json();

        // Display the server's response in the result span.
        resultSpan.textContent = result.result;
        resultSpan.className = 'result-message success';

    } catch (error) {
        // Handle errors and display them in the result span.
        resultSpan.textContent = 'A JavaScript error occurred: ' + error.message;
        resultSpan.className = 'result-message error';
    }
}

/**
 * Sends a request to destroy the network and handles the server's response.
 * Updates the UI with the operation's progress or error messages.
 */
async function destroyNetwork() {
    const resultSpan = document.getElementById('destroy-result');
    resultSpan.textContent = 'Processing...';
    resultSpan.className = 'result-message processing';

    try {
        const response = await fetch('http://127.0.0.1:5000/destroy', {
            method: 'POST'
        });

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        const result = await response.json();

        // Check the status and update the UI accordingly.
        if (result.status === 'success') {
            resultSpan.textContent = result.result;
            resultSpan.className = 'result-message success';
        } else {
            resultSpan.textContent = result.result;
            resultSpan.className = 'result-message failure';
        }

    } catch (error) {
        // Handle network or unexpected errors.
        resultSpan.textContent = 'Error occurred: ' + error.message;
        resultSpan.className = 'result-message error';
    }
}


/**
 * Fetches hosts, routers, and their interfaces from the server.
 * Populates dropdown menus with the retrieved data and adds event listeners for router selection.
 */
async function get_ips() {
    try {
        // Display a loading message.
        document.getElementById("waiting_iperf").hidden = false;
        document.getElementById("waiting_iperf").textContent = "Waiting for the fetching operation...";

        // Fetch the list of hosts from the server.
        const response_hosts = await fetch('http://127.0.0.1:5000/ips', {
            method: 'POST'
        });
        if (!response_hosts.ok) {
            throw new Error(`HTTP error! status: ${response_hosts.status}`);
        }

        const result_hosts = await response_hosts.json();

        // Fetch the list of routers and their interfaces from the server.
        const response_router = await fetch('http://127.0.0.1:5000/router_and_hosts_interfaces', {
            method: 'POST'
        });
        if (!response_router.ok) {
            throw new Error(`HTTP error! status: ${response_router.status}`);
        }

        const result_router = await response_router.json();

        // Populate the dropdown menus for hosts.
        populateDropdown("host1", result_hosts.result);
        populateDropdown("host2", result_hosts.result);

        // Populate the dropdown menu for routers.
        populateDropdown("router_dropdown", result_router);

        // Add an event listener to load router interfaces when a router is selected.
        document.getElementById("router_dropdown").addEventListener('change', function () {
            const selectedRouter = this.value;
            populateRouterInterfaces(selectedRouter, result_router);
        });

        // Make the dropdown container visible.
        document.getElementById("dropdown-container").style.display = "block";

        // Hide the loading message.
        document.getElementById("waiting_iperf").hidden = true;

    } catch (error) {
        // Handle errors and display them in the result span.
        const resultSpan = document.getElementById('ips-result');
        resultSpan.textContent = 'A JavaScript error occurred: ' + error.message;
        resultSpan.className = 'result-message error';
    }
}

/**
 * Populates a dropdown menu with options based on a given object of items.
 * @param {string} dropdownId - The ID of the dropdown element to populate.
 * @param {Object} items - An object where keys represent the options to add.
 */
function populateDropdown(dropdownId, items) {
    const dropdown = document.getElementById(dropdownId);
    dropdown.innerHTML = "";  // Clear the dropdown content.

    // Add the default "Choose" option.
    const defaultOption = document.createElement("option");
    defaultOption.value = "";
    defaultOption.textContent = `Choose ${dropdownId === "host1" || dropdownId === "host2" ? "Host" : "Router"}`;
    dropdown.appendChild(defaultOption);

    // Add each item from the object as an option in the dropdown.
    Object.entries(items).forEach(([key]) => {
        const option = document.createElement("option");
        option.value = key; // Use the key as the value.
        option.textContent = `${key}`; // Display only the key.
        dropdown.appendChild(option);
    });
}

/**
 * Populates the dropdown menu with interfaces for a selected router.
 * @param {string} routerName - The name of the router.
 * @param {Object} routerData - An object mapping router names to their interfaces.
 */
function populateRouterInterfaces(routerName, routerData) {
    const interfaceDropdown = document.getElementById("router_interface_dropdown");
    interfaceDropdown.innerHTML = "";  // Clear the dropdown content.

    // Add the default "Choose Router Interface" option.
    const defaultOption = document.createElement("option");
    defaultOption.value = "";
    defaultOption.textContent = "Choose Router Interface";
    interfaceDropdown.appendChild(defaultOption);

    // Populate the dropdown if the router has associated interfaces.
    if (routerData[routerName]) {
        routerData[routerName].forEach((interfaceName) => {
            const option = document.createElement("option");
            option.value = interfaceName;
            option.textContent = interfaceName;
            interfaceDropdown.appendChild(option);
        });
    }
}

/**
 * Handles the selection of hosts, router, and router interface.
 * Validates user input and displays the selected values or an error message.
 */
function handleSelection() {
    const host1 = document.getElementById("host1").value;
    const host2 = document.getElementById("host2").value;
    const router = document.getElementById("router_dropdown").value;
    const routerInterface = document.getElementById("router_interface_dropdown").value;

    // Validate that both hosts are selected.
    if (!host1 || !host2) {
        document.getElementById("ips-result").textContent = "Please select both hosts.";
        return;
    }

    // Validate that both router and interface are selected.
    if (!router || !routerInterface) {
        document.getElementById("ips-result").textContent = "Please select a router and an interface.";
        return;
    }

    // Display the selected values.
    document.getElementById("ips-result").textContent = `Selected Hosts: ${host1}, ${host2} | Router: ${router} | Interface: ${routerInterface}`;
}

/**
 * Submits the iPerf test data to a server and handles the response.
 * @param {Event} event - The form submission event.
 */
async function submit_iperf(event) {
    event.preventDefault();  // Prevent the default form submission behavior.

    const host1 = document.getElementById("host1").value;
    const host2 = document.getElementById("host2").value;
    const router = document.getElementById("router_dropdown").value;
    const routerInterface = document.getElementById("router_interface_dropdown").value;
    const transportType = document.getElementById("transport_type_dropdown").value;

    document.getElementById("client_result").hidden = false;
    document.getElementById("client_result").textContent = "Waiting for the test result...";

    // Validate that all fields are filled.
    if (!host1 || !host2 || !router || !routerInterface) {
        alert("Please fill in all fields.");
        return;
    }

    // Create the data object to send.
    const data = {
        host1: host1,
        host2: host2,
        router: router,
        routerInterface: routerInterface,
        transportType: transportType
    };

    try {
        // Send the data to the server via POST request.
        const response = await fetch('http://127.0.0.1:5000/set_iperf_test', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'  // Specify JSON content.
            },
            body: JSON.stringify(data)  // Convert the object to a JSON string.
        });

        document.getElementById("server_result").hidden = false;
        document.getElementById("tcpdump_result").hidden = false;

        if (response.ok) {
            // Parse the server's response.
            const result = await response.json();

            // Extract specific sections of the result.
            const fullResult = result.result;
            const clientSection = fullResult.match(/--- CLIENT RESULT ---[\s\S]*?(?=--- SERVER RESULT ---)/)?.[0]?.trim() || "N/A";
            const serverSection = fullResult.match(/--- SERVER RESULT ---[\s\S]*?(?=--- TCPDUMP RESULT ---)/)?.[0]?.trim() || "N/A";
            const tcpdumpSection = fullResult.match(/--- TCPDUMP RESULT ---[\s\S]*$/)?.[0]?.trim() || "N/A";

            // Display the sections in their respective DOM elements.
            document.getElementById("client_result").textContent = clientSection;
            document.getElementById("server_result").textContent = serverSection;
            document.getElementById("tcpdump_result").textContent = tcpdumpSection;
        } else {
            // Handle server response errors.
            console.error("Response error:", response.status, response.statusText);
            document.getElementById("result_iperf").innerHTML = `<pre>Response error: ${response.status} - ${response.statusText}</pre>`;
        }

    } catch (error) {
        // Handle JavaScript errors.
        document.getElementById("ips-result").textContent = 'A JavaScript error occurred: ' + error.message;
        document.getElementById("ips-result").className = 'result-message error';
    }
}

/**
 * Handles sending configuration files to the server.
 * This function uploads a selected file using a POST request and displays the result.
 * @param type -It indicates that the file sent is about general configurations or bgp configurations
 */
async function sendConfigurations(type) {
    // Get the file input element and result span for displaying feedback.
    let fileInput
    let resultSpan
    if(type === 'conf'){
       fileInput = document.getElementById('file-input1');
       resultSpan = document.getElementById('config-result');
    }else{
        fileInput = document.getElementById('file-input2');
        resultSpan = document.getElementById('bgp-result');
    }

    resultSpan.textContent = '';

    // Check if a file has been selected.
    if (!fileInput.files.length) {
        resultSpan.textContent = 'Please select a file to upload.';
        resultSpan.className = 'result-message error';
        return;
    } else {
        // Temporary feedback to indicate progress.
        resultSpan.textContent = `Cooking`;
    }

    // Prepare the file and form data for upload.
    const file = fileInput.files[0];
    const formData = new FormData();
    formData.append('networkFile', file); // Append the file to the form data.
    if(type === 'conf'){
        formData.append('type', "one"); // Include the type identifier.
    }else{
        formData.append('type', "two"); // Include the type identifier for BGP.
    }

    try {
        // Send the form data to the server via a POST request.
        const response = await fetch('http://127.0.0.1:5000/send-config', {
            method: 'POST',
            body: formData
        });

        // Check if the server response is successful.
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        // Parse the server's response.
        const result = await response.json();

        // Display appropriate feedback based on the response.
        if (result.success) {
            resultSpan.textContent = 'Configuration sent successfully!';
            resultSpan.className = 'result-message success';

        } else {
            resultSpan.textContent = `Error: ${result.error}`;
            resultSpan.className = 'result-message error';
        }
    } catch (err) {
        // Handle errors during the process.
        resultSpan.textContent = 'Error sending configurations.';
        resultSpan.className = 'result-message error';
        console.error(err);
    }
}

/**
 * Redirects the user to the topology page.
 * This function is triggered by a button click.
 */
function viewTopologyPage() {
    window.location.href = "topology.html"; // Navigate to the topology page.
}

