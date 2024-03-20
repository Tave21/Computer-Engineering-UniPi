# BeansBet
BeansBet is a betting application that enables users to place bets on their favorite teams!

## How to Test the Application

1. **Setup Redis and MongoDB Instances:**
    - Open a Redis instance on localhost using port 6379.
    - Open a MongoDB instance on localhost using port 27017.
    - If testing with MongoDB replicas, modify the `BaseMongoDAO` class (located in _src/main/java/it.unipi.dii/dao/base_).
        - Set `MONGO_PRIMARY/SECONDARY/THIRD_HOST_PORT` equal to 27018.
        - Update `MONGO_PRIMARY/SECONDARY/THIRD` with VM IPs.
        - Rename the `OpenConnection()` function to your preference (e.g., add "a" at the end) and call it as `OpenConnectiona()` and rename OpenConnectiona() as `OpenConnection()`.
        - Repeat the same for the `OpenStrictConnection()` function.
        - Change the `WriteConcern` attribute to "W3" in `OpenStrictConnectiona()` if using replicas.

2. **Generate a New Dataset**
    - Run `generationMainMongoDBDataset.java` in the "generation" folder (_src/main/java/it.unipi.dii/generation_).
    - This script will generate the Database in MongoDBCompass BeansBet database, all the collections and the following three files: `customers.java`, `matches.java`, and `slips.java`. Then running 'generationMainIndexes.java' will be created indexes.
    - Manually add these files to MongoDB (note: uploading `slips.java` with 1 million documents may be slow, we didn't upload it automatically because MongoDBCompass blocks).

3. **Testing scripts**
    - INCOMING MATCHES TEST: Run `update_matches_from_API` in the "testing" folder (_src/main/java/it.unipi.dii/_) to add incoming matches to the GUI's match page. In terminal you can see the number of matches before and after.
    - LIVE PAGE TEST: Run `from_TIMED_to_INPLAY' to add some matches in the GUI's live page.
    - SLIP VALIDITY CHECK: For testing this and the following ones you have to login with "user" as username and "user" as password. Run `insert_TIMED_Matches_and_place_slip` to add 3 matches to MongoDB that will appear in GUI's match page and it creates a Slip with bets on those 3 matches. Then run BeanBetGUI (_src/main/java/it.unipi.dii_) and go to Match page, add a bet on the match between "Test Timed Team 5" and "Test Timed Team 6". Now go to slip cart page and you can see the unconfirmed bet in the cart. You can run `insert_TIMED_Matches_go_INPLAY` and if you refresh the slip cart page (simply changing page and opening that again) you will see that the bet is disappeared: you can't confirm a Slip with a bet on a Match that is IN_PLAY in that moment.
    - SLIP VALIDITY CHECK 2: Run `insert_TIMED_Matches_and_place_slip` to add 3 matches to MongoDB that will appear in GUI's match page and it creates a Slip with bets on those 3 matches. Then run BeanBetGUI (_src/main/java/it.unipi.dii_) and go to Match page, click on a multiplier of the match between "Test Timed Team 5" and "Test Timed Team 6", don't click anything else on GUI and run `insert_TIMED_Matches_go_INPLAY`. Now open again the same GUI and try to add that bet to a new or already created Slip and the GUI will reject the operation showing an error message. This has been done because if the user don't refresh the match page he can bet on a IN_PLAY match.
    - SLIP STATUS TEST: Run `insert_TIMED_Matches_and_place_slip` and go to Confirmed Slip page, you will see the new Slip with grey name of matches (they are not yet played/finished) created by the execution. Now you can run `insert_TIMED_Matches_go_INPLAY` and you will see that the match name colors are changed according to the result set in the second executed file. One game has not been played yet so it is still grey.
    - POLL STATUS TEST: For testing the transfer from Redis to MongoDB of a poll, thus from active poll page and terminated poll page, you can simply add a new poll through admin account (username: save@gmail.com - password: save) that will finish after 2 minutes setting activation date to your actual date - 1 day + 2 minutes, date must be in this format: "2024-03-18T15:00:00Z".

4. **Periodic Updates**
    - In "periodicUpdates" folder the java file `periodicUpdates` executes two threads: the first every minute sends request to the API for updating matches status and for adding not yet inserted match, the second every 5 hours checks if there are some expired polls (so when current time is greater than ActivationDate + 1 day ), they will be deleted in Redis and added to MongoDB in polls collection.

5. **Admin**
   - If you want to test Admin functionalities there is a default admin: username "save@gmail.com", password "save". Admins can only be added manually to the database for security issues.


