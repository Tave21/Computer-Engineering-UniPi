
---

## **Steps:**
1. **Install NGINX**
2. **Configure the Load Balancer with hashing on GameID**
3. **Restart and Verify**

---

### **1. Install NGINX on the Node**
Access the container and install NGINX:
```bash
apt update && apt install -y nginx
```
Alternatively, if you're using Alpine Linux in containers:
```bash
apk add nginx
```
Start the service:
```bash
service nginx start
```

---

### **2. Configure the Load Balancer**
Edit the NGINX configuration file (usually located at `/etc/nginx/nginx.conf` or `/etc/nginx/conf.d/default.conf`).

```bash
apt-get install nano
nano ../etc/nginx/nginx.conf
```

Copy and Paste the following configuration, located at `nginx.conf`.

---

### **3. Restart and Test**
Restart NGINX to apply the changes:
```bash
service nginx restart
nginx -t && service nginx reload
```

**Test with `curl` (simulating different `GameID`s):**
```bash
curl "http://10.2.1.27/?GameID=12345"
curl "http://10.2.1.27/?GameID=67890"
```
Each `GameID` will always be handled by the same node.

--- 
