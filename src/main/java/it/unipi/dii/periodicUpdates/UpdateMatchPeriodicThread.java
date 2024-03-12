package it.unipi.dii.periodicUpdates;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;

import java.io.IOException;
import java.util.TimerTask;

public class UpdateMatchPeriodicThread extends TimerTask {
    /**
     * Script that launches the periodic update of the matches in MongoDB.
     */
    @Override
    public void run() {
        MatchMongoDBDAO ml = new MatchMongoDBDAO();
        try {
            ml.updateMatches();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
