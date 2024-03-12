# BeansBet
BeansBet is a betting application that enables users to place bets on their favorite teams!

## How to Test the Application

1. **Setup Redis and MongoDB Instances:**
    - Open a Redis instance on localhost using port 6379.
    - Open a MongoDB instance on localhost using port 27017.
    - If testing with MongoDB replicas, modify the `BaseMongoDAO` class (located in _src/main/java/it.unipi.dii/dao/base_).
        - Set `MONGO_PRIMARY/SECONDARY/THIRD_HOST_PORT` equal to 27018.
        - Update `MONGO_PRIMARY/SECONDARY/THIRD` with VM IPs.
        - Rename the `OpenConnection()` function to your preference (e.g., add "a" at the end) and call it as `OpenConnectiona() : OpenConnection()`.
        - Repeat the same for the `OpenStrictConnection()` function.
        - Change the `WriteConcern` attribute to "W3" in `OpenStrictConnectiona()` if using replicas.

2. **Generate a New Dataset:**
    - Run `Main.java` in the "generation" folder (_src/main/java/it.unipi.dii_).
    - This script will generate three files: `customers.java`, `matches.java`, and `slips.java`.
    - Manually add these files to MongoDB (note: uploading `slips.java` with 1 million documents may be slow).

3. **Test Live Matches Page:**
    - Run `Main2` in the "generation" folder to test the live matches page.
    - This main also creates indexes in MongoDB for a faster user experience with the application.

4. **Additional Testing:**
    - Run `Main` in _src/main/java/it.unipi.dii_ to see new incoming matches.
    - This main also allows testing the status of a slip and the slip validity check.
