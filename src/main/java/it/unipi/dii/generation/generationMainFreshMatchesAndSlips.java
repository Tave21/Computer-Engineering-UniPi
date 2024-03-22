package it.unipi.dii.generation;

import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.dao.mongo.SlipMongoDBDAO;
import it.unipi.dii.model.Match;

import static it.unipi.dii.utility.dateTimes.getCurrentInstant;
import static it.unipi.dii.utility.generators.randomGeneration.generateRandomNaturalNumber;
import static it.unipi.dii.utility.mongoUtility.deactivateMongoDBNotifications;

public class generationMainFreshMatchesAndSlips {
    public static void main(String[] args) {
        // Insert many matches.
        deactivateMongoDBNotifications();
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();
        final String thisInstant = getCurrentInstant().plusSeconds(500).toString();
        Match e;
        int i , c = 0;

        for(i = 0 ; i < 30 ; i++){
            e = new Match();
            if(i % 3 == 0) {
                e.setStatus("FINISHED");
                e.setHome_goals((int)generateRandomNaturalNumber(0 , 3));
                e.setAway_goals((int)generateRandomNaturalNumber(0 , 3));
            }else{
                e.setStatus("TIMED");
                e.setHome_goals(0);
                e.setAway_goals(0);
            }
            e.setTeam_home("Fresh Team " +  (c + 1));
            e.setTeam_away("Fresh Team " +  (c + 2));

            e.setMatchDate(thisInstant);

            if(i < 7) {
                e.setCompetition_id("IT1");
            }else if(i < 15){
                e.setCompetition_id("GB1");
            }else if(i < 20){
                e.setCompetition_id("ES1");
            }else if(i < 25){
                e.setCompetition_id("FR1");
            }else{
                e.setCompetition_id("L1");
            }
            e.initializeAndRandomizeMultipliers();
            ms.addMatch(e);
            c = c + 2;
        }
        ms.closeConnection();

        SlipMongoDBDAO sl = new SlipMongoDBDAO();
        sl.openConnection();


        sl.closeConnection();

    }
}
