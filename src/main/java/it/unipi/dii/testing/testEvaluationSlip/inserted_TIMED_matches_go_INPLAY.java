package it.unipi.dii.testing.testEvaluationSlip;

import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.model.Match;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.MongoUtility.deactivateMongoDBNotifications;

public class inserted_TIMED_matches_go_INPLAY {
    public static void main(String[] args) throws IOException {
        deactivateMongoDBNotifications();
        final String thisInstant = "2024-03-12T19:00:00Z";
        MatchMongoDBDAO mDAO = new MatchMongoDBDAO();
        mDAO.openConnection();

        // Those 3 matches go "IN_PLAY".
        List<Match> ml = new ArrayList<>();
        Match e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Test Timed Team 1");
        e.setTeam_away("Test Timed Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Test Timed Team 3");
        e.setTeam_away("Test Timed Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Test Timed Team 5");
        e.setTeam_away("Test Timed Team 6");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            mDAO.updateMatches(ml);
        } catch (
                IOException ex) {
            throw new RuntimeException(ex);
        }

        // 2 of the 3 matches go "FINISHED".

        ml = new ArrayList<>();
        e = new Match();
        e.setStatus("FINISHED");
        e.setTeam_home("Test Timed Team 1");
        e.setTeam_away("Test Timed Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1); // The 1° bet should have status = 1.
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setStatus("FINISHED");
        e.setTeam_home("Test Timed Team 3");
        e.setTeam_away("Test Timed Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(1); // The 2° bet should have status = 1.
        e.setCompetition_id("IT1");
        ml.add(e);

        // The 3° bet should have status = -1.

        try {
            mDAO.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        mDAO.closeConnection();
    }

}
