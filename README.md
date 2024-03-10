# BeansBet
BeansBet is a betting application where you can place bets on your favourite teams!
# How can you test this application?
First of all you need open a Redis and MongoDB instance in localhost:
6379 is the port for redis, 27017 is the port for mongodb. If you want to test replicas of mongodb you have to change
BaseMongoDAO (you can find it in src/main/java/it.unipi.dii/dao/base) putting MONGO_PRIMARY/SECONDARY/THIRD_HOST_PORT equal to 27018
and update MONGO_PRIMARY/SECONDARY/THIRD equal to VM IPs but also you have to change the name of OpenConnection() function, calling it
as you prefer (for example adding an "a" at the end of the name) and call OpenConnectiona() : OpenConnection().
You also have to do this for OpenStrictConnection() function.
You have also to change WriteConcern attribute to "W3" in OpenStrictConnection/na if you are using replicas.
Then you have to generate the new dataset, in order to do this you have to run Main.java under "generation" folder (you can find it in src/main/java/it.unipi.dii), this script will yield 3 files: customers.java, matches.java and slips.java and they have to be added in mongodb manually (slips.java has 1 Million documents so it is slow in uploading).
You have to run Main2 (under the same folder) in order to test live matches page and to see new incoming matches in match page. This main also will create indexes in MongoDB for a faster experience with our application.
You can also see new incoming matches running Main under src/main/java/it.unipi.dii that allows also to test status of a slip and the slip validity check.
