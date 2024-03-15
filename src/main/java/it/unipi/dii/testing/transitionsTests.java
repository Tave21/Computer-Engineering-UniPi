package it.unipi.dii.testing;

import it.unipi.dii.dao.mongo.CustomerMongoDBDAO;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.dao.mongo.PollMongoDBDAO;
import it.unipi.dii.dao.mongo.SlipMongoDBDAO;
import it.unipi.dii.model.*;
import org.bson.Document;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.DateTimes.getCurrentInstant;
import static it.unipi.dii.utility.DateTimes.getCurrentInstantString;
import static it.unipi.dii.utility.PasswordGenerator.generateRandomPassword;
import static it.unipi.dii.utility.Security.calculateSHA256;
import static it.unipi.dii.utility.randomGeneration.generatePhoneNumber;
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
        e.setTeam_home("Timed-Postponed Team 1 - " + numMatchesInit);
        e.setTeam_away("Timed-Postponed Team 2 - " + numMatchesInit);
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
        e.setTeam_home(eprev.getTeam_home());
        e.setTeam_away(eprev.getTeam_away());
        e.setMatchDate(newInstant);
        e.setHome_goals(eprev.getHome_goals());
        e.setAway_goals(eprev.getAway_goals());
        e.setCompetition_id(eprev.getCompetition_id());
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

        assertEquals("TIMED", ml.get(0).getStatus());
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

        Match e1 = new Match();
        e1.setStatus("TIMED");
        e1.setTeam_home("Timed-InPlay Team 1");
        e1.setTeam_away("Timed-InPlay Team 2");
        e1.setMatchDate(thisInstant);
        e1.setHome_goals(0); // 1 - 0
        e1.setAway_goals(0);
        e1.setCompetition_id("IT1");
        e1.setMultiplier(0 , "1" , 2);
        ml.add(e1);

        Match eref = e1;

        Match e2 = new Match();
        e2.setStatus("TIMED");
        e2.setTeam_home("Timed-InPlay Team 3");
        e2.setTeam_away("Timed-InPlay Team 4");
        e2.setMatchDate(thisInstant);
        e2.setHome_goals(0); // 1 - 1
        e2.setAway_goals(0);
        e2.setCompetition_id("IT1");
        e2.setMultiplier(2 , "X" , 2);
        ml.add(e2);

        Match e3 = new Match();
        e3.setStatus("TIMED");
        e3.setTeam_home("Timed-InPlay Team 5");
        e3.setTeam_away("Timed-InPlay Team 6");
        e3.setMatchDate(thisInstant);
        e3.setHome_goals(0); // will not finish.
        e3.setAway_goals(0);
        e3.setCompetition_id("IT1");
        ml.add(e3);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        Bet b;
        List<Bet> betList;
        Slip s;

        int numMatchesNow = ms.getLastID();
        assertEquals(numMatchesNow, numMatchesInit + ml.size()); // I inserted 3 new matches.

        SlipMongoDBDAO sDAO = new SlipMongoDBDAO();
        sDAO.openConnection();

        final int numSlipInit = sDAO.getLastID();

        s = new Slip(); // win.
        s.setUsername("Alessio_Rossi_00");
        s.setBetAmount(10);
        betList = new ArrayList<>();
        b = new Bet(
                numMatchesNow - 2,
                e1.pickMultiplierValue(0), // 1
                e1.pickMultiplierName(0),
                e1.getMatchDate()
        );
        b.setTeamHome(e1.getTeam_home());
        b.setTeamAway(e1.getTeam_away());
        b.setCompetition_id(e1.getCompetition_id());
        betList.add(b);
        b = new Bet(
                numMatchesNow - 1,
                e2.pickMultiplierValue(2), // X
                e2.pickMultiplierName(2),
                e2.getMatchDate()
        );
        b.setTeamHome(e2.getTeam_home());
        b.setTeamAway(e2.getTeam_away());
        b.setCompetition_id(e2.getCompetition_id());
        betList.add(b);
        s.setBetsList(betList);
        s.setCreationDate(getCurrentInstant().minusSeconds(20).toString());
        s.setConfirmationDate(getCurrentInstant().minusSeconds(12).toString());
        sDAO.addSlip(s);

        s = new Slip(); // lose
        s.setUsername("Alessio_Rossi_00");
        s.setBetAmount(10);
        betList = new ArrayList<>();
        b = new Bet(
                numMatchesNow - 2,
                e1.pickMultiplierValue(1), // 2 - this will lose.
                e1.pickMultiplierName(1),
                e1.getMatchDate()
        );
        b.setTeamHome(e1.getTeam_home());
        b.setTeamAway(e1.getTeam_away());
        b.setCompetition_id(e1.getCompetition_id());
        betList.add(b);
        b = new Bet(
                numMatchesNow - 1,
                e2.pickMultiplierValue(2), // X - this will win
                e2.pickMultiplierName(2),
                e2.getMatchDate()
        );
        b.setTeamHome(e2.getTeam_home());
        b.setTeamAway(e2.getTeam_away());
        b.setCompetition_id(e2.getCompetition_id());
        betList.add(b);
        s.setBetsList(betList);
        s.setCreationDate(getCurrentInstant().minusSeconds(10).toString());
        s.setConfirmationDate(getCurrentInstant().minusSeconds(2).toString());
        sDAO.addSlip(s);

        s = new Slip(); // not valuable, because the third match will not finish in this script.
        s.setUsername("Alessio_Rossi_00");
        s.setBetAmount(10);
        betList = new ArrayList<>();
        b = new Bet(
                numMatchesNow - 2,
                e1.pickMultiplierValue(0), // this will win.
                e1.pickMultiplierName(0),
                e1.getMatchDate()
        );
        b.setTeamHome(e2.getTeam_home());
        b.setTeamAway(e2.getTeam_away());
        b.setCompetition_id(e2.getCompetition_id());
        betList.add(b);
        b = new Bet(
                numMatchesNow - 1, // will lose
                e2.pickMultiplierValue(0),
                e2.pickMultiplierName(0),
                e2.getMatchDate()
        );
        b.setTeamHome(e1.getTeam_home());
        b.setTeamAway(e1.getTeam_away());
        b.setCompetition_id(e1.getCompetition_id());
        betList.add(b);
        b = new Bet(
                numMatchesNow, // not evaluated.
                e3.pickMultiplierValue(1),
                e3.pickMultiplierName(1),
                e3.getMatchDate()
        );
        b.setTeamHome(e1.getTeam_home());
        b.setTeamAway(e1.getTeam_away());
        b.setCompetition_id(e1.getCompetition_id());
        betList.add(b);
        s.setBetsList(betList);
        s.setCreationDate(getCurrentInstant().minusSeconds(40).toString());
        s.setConfirmationDate(getCurrentInstant().minusSeconds(32).toString());
        sDAO.addSlip(s);

        final int numSlipNow = sDAO.getLastID();
        assertEquals(numSlipNow, numSlipInit + 3); // the number of new inserted slip is correct.

        // Checks on the just inserted slips.
        s = sDAO.getSlip(numSlipNow - 2);
        assertNotNull(s);
        assertEquals(-1, (int) s.getWin());
        assertEquals(-1, (int) s.betsList.get(0).getWin());
        assertEquals(-1, (int) s.betsList.get(1).getWin());
        assertEquals(0, (long) s.getAmount());

        s = sDAO.getSlip(numSlipNow - 1);
        assertNotNull(s);
        assertEquals(-1, (int) s.getWin());
        assertEquals(-1, (int) s.betsList.get(0).getWin());
        assertEquals(-1, (int) s.betsList.get(1).getWin());
        assertEquals(0, (long) s.getAmount());

        s = sDAO.getSlip(numSlipNow);
        assertNotNull(s);
        assertEquals(-1, (int) s.getWin());
        assertEquals(-1, (int) s.betsList.get(0).getWin());
        assertEquals(-1, (int) s.betsList.get(1).getWin());
        assertEquals(-1, (int) s.betsList.get(2).getWin());
        assertEquals(0, (long) s.getAmount());

        assertNull(sDAO.getSlip(numSlipNow + 1));

        ml.clear();
        e1 = new Match();
        e1.setStatus("IN_PLAY");
        e1.setTeam_home("Timed-InPlay Team 1");
        e1.setTeam_away("Timed-InPlay Team 2");
        e1.setMatchDate(thisInstant);
        e1.setHome_goals(0);
        e1.setAway_goals(0);
        e1.setCompetition_id("IT1");
        ml.add(e1);

        e2 = new Match();
        e2.setStatus("IN_PLAY");
        e2.setTeam_home("Timed-InPlay Team 3");
        e2.setTeam_away("Timed-InPlay Team 4");
        e2.setMatchDate(thisInstant);
        e2.setHome_goals(0);
        e2.setAway_goals(0);
        e2.setCompetition_id("IT1");
        ml.add(e2);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        assertEquals(numMatchesNow, (int) ms.getLastID());

        e1 = ms.getMatch(numMatchesNow - 2);
        assertEquals("IN_PLAY", e1.getStatus());
        assertEquals(0, (long) e1.getHome_goals());
        assertEquals(0, (long) e1.getAway_goals());

        e2 = ms.getMatch(numMatchesNow - 1);
        assertEquals("IN_PLAY", e2.getStatus());
        assertEquals(0, (long) e2.getHome_goals());
        assertEquals(0, (long) e2.getAway_goals());

        e3 = ms.getMatch(numMatchesNow);
        assertEquals("TIMED", e3.getStatus());
        assertEquals(0, (long) e3.getHome_goals());
        assertEquals(0, (long) e3.getAway_goals());

        ml.clear();
        e1 = new Match();
        e1.setStatus("IN_PLAY");
        e1.setTeam_home("Timed-InPlay Team 1");
        e1.setTeam_away("Timed-InPlay Team 2");
        e1.setMatchDate(thisInstant);
        e1.setHome_goals(1);
        e1.setAway_goals(0);
        e1.setCompetition_id("IT1");
        ml.add(e1);

        e2 = new Match();
        e2.setStatus("IN_PLAY");
        e2.setTeam_home("Timed-InPlay Team 3");
        e2.setTeam_away("Timed-InPlay Team 4");
        e2.setMatchDate(thisInstant);
        e2.setHome_goals(1);
        e2.setAway_goals(0);
        e2.setCompetition_id("IT1");
        ml.add(e2);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        assertEquals(numMatchesNow, (int) ms.getLastID());

        e1 = ms.getMatch(numMatchesNow - 2);
        assertEquals("IN_PLAY", e1.getStatus());
        assertEquals(1, (long) e1.getHome_goals());
        assertEquals(0, (long) e1.getAway_goals());

        e2 = ms.getMatch(numMatchesNow - 1);
        assertEquals("IN_PLAY", e2.getStatus());
        assertEquals(1, (long) e2.getHome_goals());
        assertEquals(0, (long) e2.getAway_goals());

        e3 = ms.getMatch(numMatchesNow);
        assertEquals("TIMED", e3.getStatus());
        assertEquals(0, (long) e3.getHome_goals());
        assertEquals(0, (long) e3.getAway_goals());

        ml.clear();
        e1 = new Match();
        e1.setStatus("PAUSED");
        e1.setTeam_home("Timed-InPlay Team 1");
        e1.setTeam_away("Timed-InPlay Team 2");
        e1.setMatchDate(thisInstant);
        e1.setHome_goals(1);
        e1.setAway_goals(0);
        e1.setCompetition_id("IT1");
        ml.add(e1);

        e2 = new Match();
        e2.setStatus("PAUSED");
        e2.setTeam_home("Timed-InPlay Team 3");
        e2.setTeam_away("Timed-InPlay Team 4");
        e2.setMatchDate(thisInstant);
        e2.setHome_goals(1);
        e2.setAway_goals(0);
        e2.setCompetition_id("IT1");
        ml.add(e2);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        assertEquals(numMatchesNow, (int) ms.getLastID());

        e1 = ms.getMatch(numMatchesNow - 2);
        assertEquals("PAUSED", e1.getStatus());
        assertEquals(1, (long) e1.getHome_goals());
        assertEquals(0, (long) e1.getAway_goals());

        e2 = ms.getMatch(numMatchesNow - 1);
        assertEquals("PAUSED", e2.getStatus());
        assertEquals(1, (long) e2.getHome_goals());
        assertEquals(0, (long) e2.getAway_goals());

        e3 = ms.getMatch(numMatchesNow);
        assertEquals("TIMED", e3.getStatus());
        assertEquals(0, (long) e3.getHome_goals());
        assertEquals(0, (long) e3.getAway_goals());

        ml.clear();
        e1 = new Match();
        e1.setStatus("IN_PLAY");
        e1.setTeam_home("Timed-InPlay Team 1");
        e1.setTeam_away("Timed-InPlay Team 2");
        e1.setMatchDate(thisInstant);
        e1.setHome_goals(1);
        e1.setAway_goals(0);
        e1.setCompetition_id("IT1");
        ml.add(e1);

        e2 = new Match();
        e2.setStatus("IN_PLAY");
        e2.setTeam_home("Timed-InPlay Team 3");
        e2.setTeam_away("Timed-InPlay Team 4");
        e2.setMatchDate(thisInstant);
        e2.setHome_goals(1);
        e2.setAway_goals(0);
        e2.setCompetition_id("IT1");
        ml.add(e2);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        assertEquals(numMatchesNow, (int) ms.getLastID());

        e1 = ms.getMatch(numMatchesNow - 2);
        assertEquals("IN_PLAY", e1.getStatus());
        assertEquals(1, (long) e1.getHome_goals());
        assertEquals(0, (long) e1.getAway_goals());

        e2 = ms.getMatch(numMatchesNow - 1);
        assertEquals("IN_PLAY", e2.getStatus());
        assertEquals(1, (long) e2.getHome_goals());
        assertEquals(0, (long) e2.getAway_goals());

        e3 = ms.getMatch(numMatchesNow);
        assertEquals("TIMED", e3.getStatus());
        assertEquals(0, (long) e3.getHome_goals());
        assertEquals(0, (long) e3.getAway_goals());

        ml.clear();
        e1 = new Match();
        e1.setStatus("IN_PLAY");
        e1.setTeam_home("Timed-InPlay Team 1");
        e1.setTeam_away("Timed-InPlay Team 2");
        e1.setMatchDate(thisInstant);
        e1.setHome_goals(1);
        e1.setAway_goals(0);
        e1.setCompetition_id("IT1");
        ml.add(e1);

        e2 = new Match();
        e2.setStatus("IN_PLAY");
        e2.setTeam_home("Timed-InPlay Team 3");
        e2.setTeam_away("Timed-InPlay Team 4");
        e2.setMatchDate(thisInstant);
        e2.setHome_goals(1);
        e2.setAway_goals(1);
        e2.setCompetition_id("IT1");
        ml.add(e2);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        assertEquals(numMatchesNow, (int) ms.getLastID());

        e1 = ms.getMatch(numMatchesNow - 2);
        assertEquals("IN_PLAY", e1.getStatus());
        assertEquals(1, (long) e1.getHome_goals());
        assertEquals(0, (long) e1.getAway_goals());

        e2 = ms.getMatch(numMatchesNow - 1);
        assertEquals("IN_PLAY", e2.getStatus());
        assertEquals(1, (long) e2.getHome_goals());
        assertEquals(1, (long) e2.getAway_goals());

        e3 = ms.getMatch(numMatchesNow);
        assertEquals("TIMED", e3.getStatus());
        assertEquals(0, (long) e3.getHome_goals());
        assertEquals(0, (long) e3.getAway_goals());

        ml.clear();
        e1 = new Match();
        e1.setStatus("FINISHED");
        e1.setTeam_home("Timed-InPlay Team 1");
        e1.setTeam_away("Timed-InPlay Team 2");
        e1.setMatchDate(thisInstant);
        e1.setHome_goals(1);
        e1.setAway_goals(0);
        e1.setCompetition_id("IT1");
        ml.add(e1);

        e2 = new Match();
        e2.setStatus("FINISHED");
        e2.setTeam_home("Timed-InPlay Team 3");
        e2.setTeam_away("Timed-InPlay Team 4");
        e2.setMatchDate(thisInstant);
        e2.setHome_goals(1);
        e2.setAway_goals(1);
        e2.setCompetition_id("IT1");
        ml.add(e2);

        try {
            ms.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        assertEquals(numMatchesNow, (int) ms.getLastID());

        e1 = ms.getMatch(numMatchesNow - 2);
        assertEquals("FINISHED", e1.getStatus());
        assertEquals(1, (long) e1.getHome_goals());
        assertEquals(0, (long) e1.getAway_goals());

        e2 = ms.getMatch(numMatchesNow - 1);
        assertEquals("FINISHED", e2.getStatus());
        assertEquals(1, (long) e2.getHome_goals());
        assertEquals(1, (long) e2.getAway_goals());

        e3 = ms.getMatch(numMatchesNow);
        assertEquals("TIMED", e3.getStatus());
        assertEquals(0, (long) e3.getHome_goals());
        assertEquals(0, (long) e3.getAway_goals());

        assertEquals(numSlipNow, numSlipInit + 3); // The slip number does not change.

        // Check every slip evolution.

        s = sDAO.getSlip(numSlipNow - 2 );
        // Checks on the just inserted slip.
        assertNotNull(s);
        assertEquals(1, (int) s.getWin());
        assertEquals(1, (int) s.betsList.get(0).getWin());
        assertEquals(1, (int) s.betsList.get(1).getWin());
        assertEquals(40, (long) s.getAmount());

        s = sDAO.getSlip(numSlipNow - 1 );
        // Checks on the just inserted slip.
        assertNotNull(s);
        assertEquals(0, (int) s.getWin());
        assertEquals(0, (int) s.betsList.get(0).getWin());
        assertEquals(1, (int) s.betsList.get(1).getWin());
        assertEquals(0, (long) s.getAmount());

        s = sDAO.getSlip(numSlipNow);
        // Checks on the just inserted slip.
        assertNotNull(s);
        assertEquals(0, (int) s.getWin());
        assertEquals(1, (int) s.betsList.get(0).getWin());
        assertEquals(0, (int) s.betsList.get(1).getWin());
        assertEquals(-1, (int) s.betsList.get(2).getWin());
        assertEquals(0, (long) s.getAmount());

        ms.closeConnection();
        sDAO.closeConnection();
    }

    public static void testCustomers(){
        Customer c = new Customer();
        c.setName("name test");
        c.setSurname("surname test");
        c.setGender("M");
        c.setCredit(20); // Generate the credit of the user with an exponential distribution.
        c.setEmail("test@gamil.com");
        c.setCellNumber(generatePhoneNumber("+39")); // Generate the phone number.
        c.setPassword(calculateSHA256("test")); // Compute the hash of the password.
        c.setUsername("test" + generateRandomPassword(2)); // Generate the username.
        c.setBirthDate("1999-01-01");
        c.setRegistrationDate(getCurrentInstantString());
        c.setAddress("Via Onerous 2");
        c.setCityOfResidence("Pisa");
        c.setProvince("Pisa");

        CustomerMongoDBDAO cs = new CustomerMongoDBDAO();
        cs.openConnection();

        Customer c1 = cs.fetchCustomerInformation(c.getUsername());
        assertNull(c1);
        cs.registerCustomer(c);

        c1 = cs.fetchCustomerInformation(c.getUsername());
        assertNotNull(c1);
        assertEquals(c1.toString() , c.toString());

        cs.removeCustomer(c.getUsername());
        c1 = cs.fetchCustomerInformation(c.getUsername());
        assertNull(c1);
        cs.closeConnection();
    }

    public static void testPolls(){
        PollMongoDBDAO pl = new PollMongoDBDAO();
        pl.openConnection();

        final int numInit = pl.getLastID();

        Poll p = new Poll();
        p.setPollType("Best Player");
        p.setPollName("Best players of this season");
        p.setCreationDate(getCurrentInstantString());
        p.setActivationDate(getCurrentInstant().plusSeconds(3600).toString());
        List<pollOption> options = new ArrayList<>();
        pollOption pollOpt = new pollOption("Haaland");
        pollOpt.multipleVoteOption(10);
        options.add(pollOpt);
        pollOpt = new pollOption("Verstappen");
        pollOpt.multipleVoteOption(10);
        options.add(pollOpt);
        pollOpt = new pollOption("Taverna");
        pollOpt.multipleVoteOption(10);
        options.add(pollOpt);
        p.setOptions(options);
        p.UpdateNumberOfVotes();
        pl.addPoll(p);

        int numNew = pl.getLastID();

        Poll p1 = pl.getPolls(
                new Document("pollID" , new Document("$eq",  numNew)),
                new Document("_id" , 0L)
        ).get(0);

        assertEquals(numNew , numInit + 1);
        assertNotNull(p1);
        assertEquals(p1.toString() , p.toString());

        pl.removePoll(p1.getPollID());

        List<Poll> plist = pl.getPolls(
                new Document("pollID" , new Document("$eq",  numNew)),
                new Document("_id" , 0L)
        );
        assertEquals(0 , plist.size());
    }
}
