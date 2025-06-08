## Folder saving
The folder "IoT Project" must be saved in "contiki-ng/examples/" path

## Installation
The system runs on Ubuntu(64 bit), here are the installation steps:
1. Configure the MySQL Credentials in "config.ini" and "run.sh".
2. Install the python requirements:
   cd Implementation
   pip install -r requirements.txt
   cd ..

## Usage
To simplify the usage of the system the bash script "run.sh" implements various commands; 
here will be shown only the necessary ones, but it is possible to see them all running:
    ./run.sh
The system can be both simulated on *Cooja* and deployed on *nRF52840 dongle* hardware.

### Simulation
To run the simulation on cooja (you can test nodes, to test the Application you need Dongles):
1. Run:
    ./run.sh sim
2. Load "Node-Simulation_fun.csc" from Cooja interface.
    File -> Open simulation -> Select "Node-Simulation_fun.csc"
3. Click any key to start border-router and again to start CoAP Server.
4. Click "Start" in the Cooja Interface

### nRF52840 dongle

To flash the project to the nRF52840 dongle:
1. Connect 3 dongles, from top to bottom (follow this order)
2. Select each one in the USB section of the VM to plug them in
3. Run:
  ./run.sh flash
4. Reconnect each dongle, from bottom to top (follow this order)
5. Run again:
  ./run.sh flash
   Now two dongles will have the yellow led on and one no led on (this is the border router)
6. Reconnect only the third one in the list (last one from top, first one from bottom)
7. Run:
  ./run.sh deploy
8. Insert the system password for the border-router
9. Come back to the main window and click any key to run CoAP Server and User Application
10. Now you will have 3 windows: one to input commands (User Application), one to see border router output and
    one to see CoAP Server output
11. Wait a bit to see node registrations and samplings

