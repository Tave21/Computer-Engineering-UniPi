import yaml
from jinja2 import Environment, FileSystemLoader
import os

def create_file(yaml_file_path):
    # Set up the Jinja2 environment to load templates from the current directory
    #Change the current directory from web_interface to template
    actual_path = os.getcwd()   #get the current directory
    new_path = actual_path.replace("web_interface", "template")     #replace directory
    os.chdir(new_path) #load the new directory

    env = Environment(loader=FileSystemLoader('./'))
    #print("Current working directory:", os.getcwd())
    # Load the Jinja2 template for configuration
    template = env.get_template('template.j2')
    #print("carica il template")
    # Load the YAML file containing device configurations
    with open(yaml_file_path) as yaml_file:
        devices = yaml.safe_load(yaml_file)  # Parse YAML file into a Python dictionary

    #print("carica il file yaml")
    # List to store the names of the generated configuration files
    generated_files = []

    # Iterate over each device and generate configuration files
    for device in devices.get('devices', []):  # Use .get() to avoid KeyError if 'devices' is missing
        device_as_list = [{'key': key, 'value': value} for key, value in device.items()]

        # Render the template with the current device's configuration (now a list of dictionaries)
        output = template.render(device=device)

        # Generate a file name based on the device hostname
        filename = f"{device.get('hostname', 'default')}_configuration.txt"  # Default to 'default' if 'hostname' is missing

        # Write the generated configuration to a text file
        with open(os.path.join("configurations" , filename), 'w') as config_file:
            config_file.write(output)

        # Print a message indicating the configuration file has been created
        #print(f"Configuration generated for {device.get('hostname', 'default')} in {filename}")

        # Add the generated file name to the list
        generated_files.append(filename)

    return generated_files

if __name__ == "__main__":
    # Define the path to the test YAML file
    yaml_file_path = "network_configuration_and_topology/network_configuration_task2_old.yaml"

    # Check if the YAML file exists before proceeding
    if not os.path.exists(yaml_file_path):
        print(f"Error: The file {yaml_file_path} does not exist.")
    else:
        # Call the create_file function
        try:
            generated_files = create_file(yaml_file_path)
            print("Generated configuration files:")
            for file in generated_files:
                print(f" - {file}")
        except Exception as e:
            print(f"An error occurred during file generation: {e}")