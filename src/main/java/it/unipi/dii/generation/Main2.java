package it.unipi.dii.generation;
import it.unipi.dii.model.*;
import java.io.IOException;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import java.util.ArrayList;
import java.util.List;
import static it.unipi.dii.utility.DateTimes.*;
import static it.unipi.dii.utility.MongoUtility.*;

public class Main2 {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();

        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection(); // Get new matches update.

        System.out.println("Before the matches update: " + ms.getLastID());
        /*
        try {
            ms.updateMatches();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
         */

        final String thisInstant = getCurrentInstant().plusSeconds(500).toString();

        Match e = new Match();
        List<Match> ml = new ArrayList<>();
        int matchIdtemp = ms.getLastID();

        e.setMatchID(matchIdtemp);
        e.setStatus("TIMED");
        e.setTeam_home("Playing Team 1");
        e.setTeam_away("Playing Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setMatchID(matchIdtemp + 1);
        e.setStatus("TIMED");
        e.setTeam_home("Playing Team 3");
        e.setTeam_away("Playing Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("GB1");
        ml.add(e);

        e = new Match();
        e.setMatchID(matchIdtemp + 2);
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
        e.setMatchID(matchIdtemp);
        e.setStatus("IN_PLAY");
        e.setTeam_home("Playing Team 1");
        e.setTeam_away("Playing Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setMatchID(matchIdtemp + 1);
        e.setStatus("IN_PLAY");
        e.setTeam_home("Playing Team 3");
        e.setTeam_away("Playing Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(1);
        e.setCompetition_id("GB1");
        ml.add(e);

        e = new Match();
        e.setMatchID(matchIdtemp + 2);
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

        indexFields[0] = "competition_id";
        createCompoundIndex(ms.mongoDB , "matches" , indexFields, indexOrder);

        // Customer indexes
        indexFields = new String[]{"username", "name", "surname"};
        indexOrder = new Integer[]{1, 1, 1};
        createCompoundIndex(ms.mongoDB , "customers" , indexFields, indexOrder);

        System.out.println("Generation of the indexes ended.");

        ms.closeConnection();
    }
}
