---

## **Steps:**
1. **Install rebar**
2. **Create a Rebar3 App**
3. **Configure the NGINX Load Balancer**
4. **Restart and Verify**

---

# 📌 1: Install Rebar3 and Create `fleetfra_chin` App in a Linux Container

## 🛠️ Step 1: Prepare the Linux Container

If you are using **Docker**, create and start a basic Erlang container:

```bash
docker run -it --name erlang_dev ubuntu:latest bash
```

Inside the container, update the package list and install necessary dependencies:

```bash
apt update && apt install -y curl wget git build-essential
```

## 📦 Step 2: Install Erlang

Rebar3 requires **Erlang** to be installed. You can install it from the official repository:

```bash
apt install -y erlang
```

Verify the installation:

```bash
erl -version
```

## 🚀 Step 3: Install Rebar3

Download and install **Rebar3**:

```bash
wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && mv rebar3 /usr/local/bin/
```

Check if the installation was successful:

```bash
rebar3 version
```

You should see output like:

```
rebar 3.20.0
```

## 📂 Step 4: Create the `fleetfra_chin` Application

Navigate to your working directory and create a new Erlang application, with umbrella structure:

```bash
mkdir /opt/fleetfra_chin
cd /opt/fleetfra_chin
rebar3 new release fleetfra_chin
```

This will generate a folder structure like:

```
fleetfra_chin/
├── apps/
├── config/
│   ├── sys.config
│   ├── vm.args
├── rebar.config
├── README.md

```

## 🛠️ Step 5: Compile and Run the App

Move into the app directory:

```bash
cd fleetfra_chin
```

Copy&Paste the content of our directory (overwrite everything).

## Step 6: Start the servers
in each Erlang node write:
```bash
cd ../FleetFra/fleetfra_chin
rebar3 clean
rebar3 shell --name fleetfra28@10.2.1.28 --setcookie 'fleetfra'
```

```bash
cd ../FleetFra/fleetfra_chin
rebar3 clean
rebar3 shell --name fleetfra29@10.2.1.29 --setcookie 'fleetfra'
```

```bash
cd ../FleetFra/fleetfra_chin
rebar3 clean
rebar3 shell --name fleetfra30@10.2.1.30 --setcookie 'fleetfra'
```