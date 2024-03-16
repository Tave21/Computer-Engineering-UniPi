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
        createIndex(ms.mongoDB , "slips" , "confirmationDate" , -1);
        createIndex(ms.mongoDB , "slips" , "username" , 1);
        createIndex(ms.mongoDB , "slips" , "betList.matchID" , -1);

        // Polls indexes.
        createIndex(ms.mongoDB , "polls" , "activationDate" , -1);
        createIndex(ms.mongoDB , "polls" , "pollType" , 1);

        // Matches indexes.
        createIndex(ms.mongoDB , "matches" , "matchDate" , -1);
        createIndex(ms.mongoDB , "matches" , "status" , 1);

        String[] indexFields = {"matchDate", "team_home" , "team_away"};
        Integer[] indexOrder = { - 1, 1 , 1};
        createCompoundIndex(ms.mongoDB , "matches" , indexFields, indexOrder);

        // Customer indexes
        indexFields = new String[]{"username", "name", "surname"};
        indexOrder = new Integer[]{1, 1, 1};
        createCompoundIndex(ms.mongoDB , "customers" , indexFields, indexOrder);

        System.out.println("Generation of the indexes ended.");

        ms.closeConnection();
    }
}
