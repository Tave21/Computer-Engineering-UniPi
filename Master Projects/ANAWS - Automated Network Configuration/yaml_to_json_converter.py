import yaml
import json


def process_topology():
    """
    Reads a YAML file and generates a JSON file with the processed network topology.

    """
    input_yaml_file = '../template/topology_task2.yaml'
    output_json_file = 'network_topology.json'

    try:
        with open(input_yaml_file, 'r') as file:
            network_topology = yaml.safe_load(file)

        nodes = network_topology['topology']['nodes']
        links = network_topology['topology']['links']

        # Replace node images
        for node_id, node_data in nodes.items():
            if node_data['image'] == 'ceos:4.33.0F':
                nodes[node_id]['image'] = 'router.png' if "sw" not in node_id else "switch.png"
            elif node_data['image'] == 'alpine:latest':
                nodes[node_id]['image'] = 'computer.png'

        # Convert links
        generated_links = []
        for link in links:
            endpoints = link['endpoints']
            src, dst = endpoints[0].split(':'), endpoints[1].split(':')

            # Keep the links unchanged
            generated_links.append({
                "endpoints": [
                    f"{src[0]}:{src[1]}",
                    f"{dst[0]}:{dst[1]}"
                ]
            })

        # Generate the final JSON file
        final_topology = {
            "nodes": [
                {"id": key, "label": key, "image": value["image"]} for key, value in nodes.items()
            ],
            "links": [
                {"from": link['endpoints'][0].split(':')[0], "to": link['endpoints'][1].split(':')[0]} for link in
                generated_links
            ]
        }

        # Write the JSON file
        with open(output_json_file, 'w') as json_file:
            json.dump(final_topology, json_file, indent=4)

        #print(f"JSON file successfully generated at {output_json_file}")
        return True

    except Exception as e:
        print(f"Error while generating the JSON file: {e}")
        return False
