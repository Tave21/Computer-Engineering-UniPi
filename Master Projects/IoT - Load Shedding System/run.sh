#---------------------------------------------
#               PARAMETERS
#---------------------------------------------
# List of nodes to compile
NODE_LIST="load_manager DR_manager"
# MySQL Credentials for creating the database
MYSQL_USER="iotuser"
MYSQL_PASSWORD="iotpassword"

#---------------------------------------------

#Utility variables
DATABASE_NAME="LoadForecastingDB"
TARGET_BOARD="TARGET=nrf52840 BOARD=dongle"
BASE_PATH="Implementation/"

function compile_node(){
    local node_name=$1
    local target="nrf52840"
    local actual_path=$(pwd)

    cd ./$BASE_PATH$node_name
    make distclean 2>&1 | grep -E "error|warning|TARGET not defined, using target 'native'" | grep -v "CC "
    make $TARGET_BOARD $node_name 2>&1 | grep -E "error|warning|TARGET not defined, using target 'native'" | grep -v "CC "
    cd "$actual_path"
}

function compile_all_nodes(){
    echo "Start compiling all the nodes:"
    for node in $NODE_LIST
    do
        echo -e "\t - Compiling ${node}..."
        compile_node $node
    done
    echo "All nodes compiled successfully!"
}

function run_cooja(){
    #./gradlew run --info --debug
    gnome-terminal -- bash -c 'cd; cd contiki-ng/tools/cooja; ./gradlew run; exec bash'
}

function run_rpl_border_router(){
    local target=$1
    if [ "$target" != "cooja" ]; then
        gnome-terminal -- bash -c ' cd ..;cd rpl-border-router;make TARGET=nrf52840 BOARD=dongle PORT=/dev/ttyACM0 connect-router;'
        echo "Connecting rpl-border-router to dongle"
    else
        gnome-terminal -- bash -c ' cd ..;cd rpl-border-router;make TARGET=cooja connect-router-cooja;'
        echo "Connecting rpl-border-router to cooja"
    fi
    
}

function run_CoAP_server(){

    gnome-terminal -- bash -c 'cd ./'$BASE_PATH'PythonApplication; python3 ./server.py; exec bash'
}

function run_user_app(){
    gnome-terminal -- bash -c 'cd ./'$BASE_PATH'PythonApplication; python3 ./userapp.py; exec bash'
}

function mysql_cmd() {
    mysql -u$MYSQL_USER -p$MYSQL_PASSWORD -e "$1" 2>&1 | grep -v "Warning: Using a password"
}

function create_db(){
    local fresh_start=$1
    # Create database
    #if [ "$fresh_start" == "fresh" ]; then
    echo "Deleting previous database..."
    mysql_cmd "DROP DATABASE IF EXISTS ${DATABASE_NAME};" > /dev/null
    #fi
    echo "Creating new database if not exists..."
    mysql_cmd "CREATE DATABASE IF NOT EXISTS ${DATABASE_NAME};" > /dev/null

    #Adding tables
    mysql_cmd "CREATE TABLE IF NOT EXISTS ${DATABASE_NAME}.nodes 
    (ip VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL, 
    resource VARCHAR(255) NOT NULL,
    settings VARCHAR(255) NOT NULL,
    PRIMARY KEY (ip));" > /dev/null
    echo -e "\t- \"nodes\" table created"

    mysql_cmd "CREATE TABLE IF NOT EXISTS ${DATABASE_NAME}.load
    (timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    sampled FLOAT DEFAULT NULL, 
    predicted FLOAT DEFAULT NULL,
    PRIMARY KEY (timestamp));" > /dev/null
    echo -e "\t- \"load\" table created"

    mysql_cmd "CREATE TABLE IF NOT EXISTS ${DATABASE_NAME}.relay (
    id INT NOT NULL AUTO_INCREMENT,
    timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    status BOOLEAN NOT NULL,
    PRIMARY KEY (id),
    KEY idx_timestamp (timestamp)
    );" > /dev/null
    echo -e "\t- \"relay\" table created"


    echo "Database created successfully!"
}

function query_db()
{
    mysql -u$MYSQL_USER -p$MYSQL_PASSWORD -D$DATABASE_NAME -e "$1"
}

# Function to flash a sensor on a specific port
function flash_sensor() {
    local node_name=$1
    local port=$2

    if [ -n "$node_name" ]; then
        echo "Flashing $node_name sensor on $port..."
        cd ./$BASE_PATH$node_name || exit 1
        make $TARGET_BOARD ${node_name}.dfu-upload PORT=$port
        cd - > /dev/null || exit 1
    else
        echo "Invalid sensor name: $node_name"
    fi
}

# Function to flash the RPL border router
function flash_rpl_border_router() {
    echo "Flashing RPL border router on /dev/ttyACM0..."
    cd ../rpl-border-router || exit 1
    make $TARGET_BOARD PORT=/dev/ttyACM0 border-router.dfu-upload
    cd - > /dev/null || exit 1
}

# Flash function for all nodes
function flash() {
    local ports=("/dev/ttyACM1" "/dev/ttyACM2")
    local index=0

    flash_rpl_border_router

    for node_name in $NODE_LIST; do
        flash_sensor "$node_name" "${ports[$index]}"
        index=$((index + 1))
    done

    
}

case $1 in
    compile_all)
        compile_all_nodes
        ;;
    compile)
        compile_node $2
        ;;
    cooja)
        run_cooja
        ;;
    border-router)
        run_rpl_border_router $2
        ;;
    coap-server)
        run_CoAP_server
        ;;
    sim)
        create_db "fresh"
        compile_all_nodes
        echo "Starting Cooja..."
        run_cooja
        echo "Press any key to start the border-router..."
        read -n 1 -s
        run_rpl_border_router "cooja"
        echo "Press any key to start the CoAP server..."
        read -n 1 -s
        run_CoAP_server
        ;;
    relsim)
        create_db "fresh"
        run_rpl_border_router "cooja"
        echo "Press any key to start the CoAP server..."
        read -n 1 -s
        run_CoAP_server
        ;;
    create-db)
        create_db "$2"
        ;;
    sql)
        query_db "$2"
        ;;
    user)
        run_user_app
        ;;
    flash)
        flash
        ;;
    deploy)
        echo "Starting deployment..."
        create_db
        run_rpl_border_router
        echo "Press any key to start the CoAP server and the User application..."
        read -n 1 -s
        run_CoAP_server
        run_user_app
        ;;
    *)
        echo "------------------------------------------------------------------HELP----------------------------------------------------------------"
        echo "Usage: $0 {compile_all|compile|cooja|border-router|coap-server|sim|relsim|create-db|sql|user|flash|deploy}"
        echo "----------------------------------------------------------------COMMANDS--------------------------------------------------------------"
        echo "[1] WRAP-UP COMMANDS:"
        echo -e "\t-sim: Resets the database, compiles the nodes, runs the Cooja simulator, connects the border router and starts the CoAP server"
        echo -e "\t-relsim: Runs the coap server and connects the border router to cooja"
        echo -e "\t-flash: Flashes the nodes to the dongles"
        echo -e "\t-deploy: Deploys the system"
        echo "[2] SINGLE COMMANDS:"
        echo -e "\t-compile_all: Compiles all the nodes"
        echo -e "\t-compile: Compiles a specific node. Usage: compile <node_name>"
        echo -e "\t-cooja: Runs the Cooja simulator"
        echo -e "\t-border-router: Runs the RPL border router. Usage: border-router <target>"
        echo -e "\t-coap-server: Runs the CoAP server"
        echo -e "\t-create-db: Creates the database if not exists. Write 'fresh' to delete the previous database. Usage: create-db <fresh>"
        echo -e "\t-sql: Executes a SQL query on the database. Usage: sql <query>"
        echo -e "\t-user: Runs the user application"

        exit 1
        ;;
esac