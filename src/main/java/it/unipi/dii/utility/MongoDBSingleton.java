package it.unipi.dii.utility;

import com.mongodb.client.MongoClient;

public class MongoDBSingleton {
    private static MongoClient mongoClient;

    private MongoDBSingleton() {
    }
    public static synchronized MongoClient getInstance() {
        if (mongoClient == null) {
            //mongoClient = new MongoClient("localhost", 27017);
        }
        return mongoClient;
    }
}
