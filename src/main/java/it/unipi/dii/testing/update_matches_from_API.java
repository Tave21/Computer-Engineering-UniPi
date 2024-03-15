package it.unipi.dii.testing;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;

import java.io.IOException;

import static it.unipi.dii.utility.MongoUtility.deactivateMongoDBNotifications;

//USED TO RETRIEVE NEW MATCHES FROM API
public class update_matches_from_API {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();
        System.out.println("Before the matches update: " + ms.getLastID());
        try {
            ms.updateMatches();
        }catch (IOException e){
            System.out.println("Failed to update matches from API!");
        }
        System.out.println("After the matches update: " + ms.getLastID());
        ms.closeConnection();
    }
}
