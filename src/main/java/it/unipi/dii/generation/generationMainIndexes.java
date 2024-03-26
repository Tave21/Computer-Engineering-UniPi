package it.unipi.dii.generation;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;

import static it.unipi.dii.utility.mongoUtility.*;

public class generationMainIndexes {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();

        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();

        System.out.println("Start the generation of the indexes.");

        // Slips indexes.
        createIndex(ms.mongoDB , "slips" , "username" , 1); // Good index.
        createIndex(ms.mongoDB , "slips" , "betList.matchID" , -1); // Good index.

        // Matches indexes.
        createIndex(ms.mongoDB , "matches" , "status" , 1); // Good Index

        System.out.println("Generation of the indexes ended.");

        ms.closeConnection();
    }
}
