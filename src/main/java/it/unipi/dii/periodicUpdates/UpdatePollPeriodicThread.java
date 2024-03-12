package it.unipi.dii.periodicUpdates;

import it.unipi.dii.dao.mongo.MatchMongoDBDAO;

import java.io.IOException;
import java.util.TimerTask;

public class UpdatePollPeriodicThread extends TimerTask {
    /**
     * Script that launches the periodic update of the polls in Redis.
     */
    @Override
    public void run() {
        System.out.println("AO POLL");

    }
}
