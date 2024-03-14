package it.unipi.dii.testing.matchesTransitionTests;

import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.dao.mongo.SlipMongoDBDAO;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Match;
import it.unipi.dii.model.Slip;
import org.bson.Document;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.DateTimes.getCurrentInstant;
import static it.unipi.dii.utility.DateTimes.getCurrentInstantString;
import static org.junit.Assert.*;

public class transitionsTests {
    public static void testTIMEDtoCANCELED() {
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();
        final String thisInstant = getCurrentInstant().plusSeconds(500).toString();
        final int numMatchesInit = ms.getLastID();
        List<Match> ml = new ArrayList<>();

        Match e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Timed-Canceled Team Stay 1");
        e.setTeam_away("Timed-Canceled Team Stay 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);
        Match eprev1 = e;

        // This match will evolve.
        e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Timed-Canceled Team 1");
        e.setTeam_away("Timed-Canceled Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);
        Match eprev2 = e;

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        int numMatchesNow = ms.getLastID();
        assertEquals(numMatchesInit + 2, numMatchesNow);

        SlipMongoDBDAO sDAO = new SlipMongoDBDAO();
        sDAO.openConnection();

        // Create a slip on this match.
        int numSlipInit = sDAO.getLastID();

        Slip s = new Slip();
        s.setUsername("Alessio_Rossi_00");
        s.setBetAmount(10);
        List<Bet> betList = new ArrayList<>();
        Bet b = new Bet(
                numMatchesNow,
                eprev2.pickMultiplierValue(0),
                eprev2.pickMultiplierName(0),
                eprev2.getMatchDate()
        );
        b.setTeamHome(eprev2.getTeam_home());
        b.setTeamAway(eprev2.getTeam_away());
        b.setCompetition_id(eprev2.getCompetition_id());
        betList.add(b);
        s.setBetsList(betList);
        s.setCreationDate(getCurrentInstant().minusSeconds(20).toString());
        s.setConfirmationDate(getCurrentInstantString());
        sDAO.addSlip(s);

        s = new Slip();
        s.setUsername("Alessio_Rossi_01");
        s.setBetAmount(12);
        betList = new ArrayList<>();

        b = new Bet(
                numMatchesNow - 1,
                eprev1.pickMultiplierValue(1),
                eprev1.pickMultiplierName(1),
                eprev1.getMatchDate()
        );
        b.setTeamHome(eprev1.getTeam_home());
        b.setTeamAway(eprev1.getTeam_away());
        b.setCompetition_id(eprev1.getCompetition_id());
        betList.add(b);

        b = new Bet(
                numMatchesNow,
                eprev2.pickMultiplierValue(2),
                eprev2.pickMultiplierName(2),
                eprev2.getMatchDate()
        );
        b.setTeamHome(eprev2.getTeam_home());
        b.setTeamAway(eprev2.getTeam_away());
        b.setCompetition_id(eprev2.getCompetition_id());
        betList.add(b);

        s.setBetsList(betList);
        s.setCreationDate(getCurrentInstant().minusSeconds(20).toString());
        s.setConfirmationDate(getCurrentInstantString());
        sDAO.addSlip(s);

        int numSlipNow = sDAO.getLastID();
        Slip slipCheck1 = sDAO.getSlip(numSlipNow - 1);
        Slip slipCheck2 = sDAO.getSlip(numSlipNow);

        assertEquals(numSlipNow, numSlipInit + 2);

        // Checks on the just inserted slip.
        assertNotNull(slipCheck1);
        assertEquals(-1, (int) slipCheck1.getWin());
        assertEquals(-1, (int) slipCheck1.betsList.get(0).getWin());
        assertEquals(0, (long) slipCheck1.getAmount());

        // Checks on the just inserted slip.
        assertNotNull(slipCheck2);
        assertEquals(-1, (int) slipCheck2.getWin());
        assertEquals(-1, (int) slipCheck2.betsList.get(0).getWin());
        assertEquals(0, (long) slipCheck2.getAmount());

        // The match is canceled.

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

        ml.clear();
        ml = ms.getMatches(
                ms.getAndCondition(eprev2, "date"),
                new Document("_id", 0L)
        );
        assertEquals(ml.size(), 0);

        slipCheck1 = sDAO.getSlip(slipCheck1.getSlipID());
        slipCheck2 = sDAO.getSlip(slipCheck2.getSlipID());

        // The first slip should be deleted.
        // The second slip should have only one bet.
        assertEquals(numSlipNow, numSlipInit + 2);

        // Checks on the previously inserted slip.
        assertNull(slipCheck1);
        assertNotNull(slipCheck2);
        assertEquals(1 , slipCheck2.betsList.size());
        assertEquals(-1, (int) slipCheck2.getWin());
        assertEquals(-1, (int) slipCheck2.betsList.get(0).getWin());
        assertEquals(0, (long) slipCheck2.getAmount());
        ms.closeConnection();
        sDAO.closeConnection();
    }

    public static void testTIMEDtoPOSTPONED() {
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();
        final String thisInstant = getCurrentInstant().plusSeconds(100).toString();
        final String newInstant = getCurrentInstant().plusSeconds(4000).toString();

        final int numMatchesInit = ms.getLastID();
        List<Match> ml = new ArrayList<>();

        Match e = new Match();

        e.setStatus("TIMED");
        e.setTeam_home("Timed-Postponed Team 1");
        e.setTeam_away("Timed-Postponed Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);
        final Match eprev = e;

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
        int numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow, numMatchesInit + 1);


        SlipMongoDBDAO sDAO = new SlipMongoDBDAO();
        sDAO.openConnection();
        int numSlipInit = sDAO.getLastID();

        Slip s = new Slip();
        s.setUsername("Alessio_Rossi_00");
        s.setBetAmount(12);
        List<Bet> betList = new ArrayList<>();
        Bet b = new Bet(
                numMatchesNow,
                eprev.pickMultiplierValue(0),
                eprev.pickMultiplierName(0),
                eprev.getMatchDate()
        );
        b.setTeamHome(eprev.getTeam_home());
        b.setTeamAway(eprev.getTeam_away());
        b.setCompetition_id(eprev.getCompetition_id());
        betList.add(b);

        s.setBetsList(betList);
        s.setCreationDate(getCurrentInstant().minusSeconds(20).toString());
        s.setConfirmationDate(getCurrentInstantString());
        sDAO.addSlip(s);

        int numSlipNow = sDAO.getLastID();

        assertEquals(numSlipNow , numSlipInit + 1);

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

        assertEquals((long) ms.getLastID(), numMatchesNow);

        ml.clear();
        ml = ms.getMatches(
                ms.getAndCondition(eprev, "date"),
                new Document("_id", 0L)
        );

        assertEquals(ml.size(), 0);

        ml.clear();
        ml = ms.getMatches(
                ms.getAndCondition(e, "date"),
                new Document("_id", 0L)
        );
        assertEquals(ml.size(), 1);

        assertEquals("POSTPONED", ml.get(0).getStatus());
        assertEquals(0, (long) ml.get(0).getHome_goals());
        assertEquals(0, (long) ml.get(0).getAway_goals());
        assertEquals(ml.get(0).getMatchDate(), newInstant);

        Slip slipCheck2 = sDAO.getSlip(numSlipNow);
        assertNotNull(slipCheck2);
        assertEquals(1 , slipCheck2.betsList.size());
        assertEquals(-1, (int) slipCheck2.getWin());
        assertEquals(-1, (int) slipCheck2.betsList.get(0).getWin());
        assertEquals(0, (long) slipCheck2.getAmount());
        assertEquals(newInstant, slipCheck2.betsList.get(0).getMatchDate());

        ms.closeConnection();
        sDAO.closeConnection();
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
        assertEquals(numMatchesNow, numMatchesInit + 1);

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
        assertEquals(numMatchesNow, numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("IN_PLAY", e.getStatus());
        assertEquals(0, (long) e.getHome_goals());
        assertEquals(0, (long) e.getAway_goals());

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
        assertEquals(numMatchesNow, numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("IN_PLAY", e.getStatus());
        assertEquals(1, (long) e.getHome_goals());
        assertEquals(1, (long) e.getAway_goals());

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
        assertEquals(numMatchesNow, numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("PAUSED", e.getStatus());
        assertEquals(1, (long) e.getHome_goals());
        assertEquals(1, (long) e.getAway_goals());

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
        assertEquals(numMatchesNow, numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("IN_PLAY", e.getStatus());
        assertEquals(1, (long) e.getHome_goals());
        assertEquals(1, (long) e.getAway_goals());

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
        assertEquals(numMatchesNow, numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("IN_PLAY", e.getStatus());
        assertEquals(2, (long) e.getHome_goals());
        assertEquals(1, (long) e.getAway_goals());

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
        assertEquals(numMatchesNow, numMatchesInit + 1);

        e = ms.getMatch(numMatchesNow);
        assertEquals("FINISHED", e.getStatus());
        assertEquals(2, (long) e.getHome_goals());
        assertEquals(1, (long) e.getAway_goals());

        ms.closeConnection();
    }

}
