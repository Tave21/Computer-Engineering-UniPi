package it.unipi.dii.periodicUpdates;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;

import java.io.IOException;
import java.util.TimerTask;

public class UpdateMatchPeriodicThread extends TimerTask {
    /**
     * Program that launches the update of the matches.
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
