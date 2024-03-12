package it.unipi.dii.testing;

import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.model.Match;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.DateTimes.getCurrentInstant;
import static it.unipi.dii.utility.MongoUtility.deactivateMongoDBNotifications;

public class from_TIMED_to_INPLAY {
    public static void main(String[] args) {
        // This script send three updates to Mongo where 3 previously inserted matches go to IN_PLAY.
        // And then to FINISHED.
        deactivateMongoDBNotifications();
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();
        final String thisInstant = getCurrentInstant().plusSeconds(500).toString();
        Match e = new Match();
        List<Match> ml = new ArrayList<>();

        System.out.println("Before the matches update: " + ms.getLastID());
        e.setStatus("TIMED");
        e.setTeam_home("Playing Team 1");
        e.setTeam_away("Playing Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Playing Team 3");
        e.setTeam_away("Playing Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("GB1");
        ml.add(e);

        e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Playing Team 5");
        e.setTeam_away("Playing Team 6");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        ml = new ArrayList<>();
        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Playing Team 1");
        e.setTeam_away("Playing Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Playing Team 3");
        e.setTeam_away("Playing Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(1);
        e.setCompetition_id("GB1");
        ml.add(e);

        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Playing Team 5");
        e.setTeam_away("Playing Team 6");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        System.out.println("After the matches update: " + ms.getLastID());
        ms.closeConnection();

    }
}
