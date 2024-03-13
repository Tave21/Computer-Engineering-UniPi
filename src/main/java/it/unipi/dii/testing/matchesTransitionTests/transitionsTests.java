package it.unipi.dii.testing.matchesTransitionTests;

import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.model.Match;
import org.bson.Document;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.DateTimes.getCurrentInstant;
import static org.junit.Assert.assertEquals;

public class transitionsTests {
    public static void testTIMEDtoCANCELED() {
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();
        final String thisInstant = getCurrentInstant().plusSeconds(500).toString();

        final int numMatchesInit = ms.getLastID();
        List<Match> ml = new ArrayList<>();

        Match e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Timed-Canceled Team 1");
        e.setTeam_away("Timed-Canceled Team 2");
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
        int numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        Match eprev = e;

        ml.clear();
        e = new Match();
        e.setStatus("CANCELED");
        e.setTeam_home("Timed-Canceled Team 1");
        e.setTeam_away("Timed-Canceled Team 2");
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

        numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit);

        ml.clear();
        ml = ms.getMatches(
                ms.getAndCondition(eprev , "date"),
                new Document("_id", 0L)
        );
        assertEquals(ml.size() , 0);
        ms.closeConnection();
    }

    public static void testTIMEDtoPOSTPONED() {
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();
        final String thisInstant = getCurrentInstant().plusSeconds(500).toString();
        final String newInstant = getCurrentInstant().plusSeconds(2000).toString();

        final int numMatchesInit = ms.getLastID();
        List<Match> ml = new ArrayList<>();

        Match e = new Match();
        Match eprev;

        e.setStatus("TIMED");
        e.setTeam_home("Timed-Postponed Team 1");
        e.setTeam_away("Timed-Postponed Team 2");
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
        int numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        eprev = e;

        ml.clear();
        e = new Match();
        e.setStatus("POSTPONED");
        e.setTeam_home("Timed-Postponed Team 1");
        e.setTeam_away("Timed-Postponed Team 2");
        e.setMatchDate(newInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        assertEquals((long) ms.getLastID() , numMatchesNow);

        ml.clear();
        ml = ms.getMatches(
                ms.getAndCondition(eprev , "date"),
                new Document("_id", 0L)
        );
        assertEquals(ml.size() , 0);

        ml.clear();
        ml = ms.getMatches(
                ms.getAndCondition(e , "date"),
                new Document("_id", 0L)
        );
        assertEquals(ml.size() , 1);

        assertEquals("POSTPONED" , ml.get(0).getStatus());
        assertEquals(0 , (long) ml.get(0).getHome_goals());
        assertEquals(0 , (long) ml.get(0).getAway_goals());
        assertEquals(ml.get(0).getMatchDate() , newInstant);
        ms.closeConnection();
    }

    public static void testTIMEDtoINPLAYtoPAUSEDtoINPLAYtoFINISHED() {
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();

        final String thisInstant = getCurrentInstant().plusSeconds(500).toString();
        List<Match> ml = new ArrayList<>();
        final int numMatchesInit = ms.getLastID();

        Match e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Timed-InPlay Team 1");
        e.setTeam_away("Timed-InPlay Team 2");
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

        int numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        ml.clear();
        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Timed-InPlay Team 1");
        e.setTeam_away("Timed-InPlay Team 2");
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

        numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("IN_PLAY" , e.getStatus());
        assertEquals(0 , (long) e.getHome_goals());
        assertEquals(0 , (long) e.getAway_goals());

        ml.clear();
        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Timed-InPlay Team 1");
        e.setTeam_away("Timed-InPlay Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("IN_PLAY" , e.getStatus());
        assertEquals(1 , (long) e.getHome_goals());
        assertEquals(1 , (long) e.getAway_goals());

        ml.clear();
        e = new Match();
        e.setStatus("PAUSED");
        e.setTeam_home("Timed-InPlay Team 1");
        e.setTeam_away("Timed-InPlay Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("PAUSED" , e.getStatus());
        assertEquals(1 , (long) e.getHome_goals());
        assertEquals(1 , (long) e.getAway_goals());

        ml.clear();
        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Timed-InPlay Team 1");
        e.setTeam_away("Timed-InPlay Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("IN_PLAY" , e.getStatus());
        assertEquals(1 , (long) e.getHome_goals());
        assertEquals(1 , (long) e.getAway_goals());

        ml.clear();
        e = new Match();
        e.setStatus("IN_PLAY");
        e.setTeam_home("Timed-InPlay Team 1");
        e.setTeam_away("Timed-InPlay Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(2);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("IN_PLAY" , e.getStatus());
        assertEquals(2 , (long) e.getHome_goals());
        assertEquals(1 , (long) e.getAway_goals());

        ml.clear();
        e = new Match();
        e.setStatus("FINISHED");
        e.setTeam_home("Timed-InPlay Team 1");
        e.setTeam_away("Timed-InPlay Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow , numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("FINISHED" , e.getStatus());
        assertEquals(2 , (long) e.getHome_goals());
        assertEquals(1 , (long) e.getAway_goals());

        ms.closeConnection();
    }

}
