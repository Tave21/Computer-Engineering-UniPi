package it.unipi.dii;

import it.unipi.dii.dao.mongo.*;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Match;
import it.unipi.dii.model.Slip;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.DateTimes.*;
import static it.unipi.dii.utility.MongoUtility.*;
import static it.unipi.dii.utility.MongoUtility.createCompoundIndex;

public class Main {
    public static void main(String[] args) throws IOException {

        deactivateMongoDBNotifications();


        Match e = new Match();
        MatchMongoDBDAO mDAO = new MatchMongoDBDAO();
        List<Match> ml = new ArrayList<>();
        final String thisInstant = getCurrentInstant().plusSeconds(500).toString();

        // Update the matches with the periodic update function.
        MatchMongoDBDAO ms = new MatchMongoDBDAO();
        ms.openConnection();
        System.out.println(ms.getLastID());
        ms.updateMatches(); // Get the new matches from the API.
        System.out.println(ms.getLastID());
        ms.closeConnection();


        //TEST FOR CONTROL STATUS OF A SLIP
        //ADD A SLIP TO CONFIRMED SLIPS with one or more matches, THEN CHANGE status of a match or more (if more than one)
        //THEN RUN THIS 4 LINES AND THEN CHECK THE STATUS OF THE SLIP IN THE DEDICATED PAGE


        mDAO.openConnection();
        e = new Match();
        e.setMatchID(mDAO.getLastID());
        e.setStatus("TIMED");
        e.setTeam_home("Test Team 1");
        e.setTeam_away("Test Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setMatchID(mDAO.getLastID());
        e.setStatus("TIMED");
        e.setTeam_home("Test Team 3");
        e.setTeam_away("Test Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setMatchID(mDAO.getLastID());
        e.setStatus("TIMED");
        e.setTeam_home("Test Team 5");
        e.setTeam_away("Test Team 6");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            mDAO.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        SlipMongoDBDAO sDAO = new SlipMongoDBDAO();
        sDAO.openConnection();

        // Create a slip on those 3 matches.

        Slip s = new Slip();
        s.setUsername("Alessio_Rossi_00");
        s.setBetAmount(10);
        List<Bet> betList = new ArrayList<>();

        for(int i = 0 ; i < ml.size() ; i++){
            Bet b = new Bet(
                    mDAO.getLastID() - i,
                    ml.get(i).pickMultiplierValue(i),
                    ml.get(i).pickMultiplierName(i),
                    ml.get(i).getMatchDate()
            );
            b.setTeamHome(ml.get(i).getTeam_home());
            b.setTeamAway(ml.get(i).getTeam_away());
            b.setCompetition_id(ml.get(i).getCompetition_id());
            betList.add(b);
        }
        s.setBetsList(betList);
        s.setConfirmationDate(getCurrentInstantString());
        s.setCreationDate(getCurrentInstant().minusSeconds(3600).toString());

        sDAO.addSlip(s);
        sDAO.closeConnection();

        //POST CREAZIONE SLIP DALLA GUI

        // Those 3 matches go "IN_PLAY".
        ml = new ArrayList<>();
        e = new Match();
        e.setMatchID(mDAO.getLastID() - 2);
        e.setStatus("IN_PLAY");
        e.setTeam_home("Test Team 1");
        e.setTeam_away("Test Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setMatchID(mDAO.getLastID() - 1);
        e.setStatus("IN_PLAY");
        e.setTeam_home("Test Team 3");
        e.setTeam_away("Test Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setMatchID(mDAO.getLastID());
        e.setStatus("IN_PLAY");
        e.setTeam_home("Test Team 5");
        e.setTeam_away("Test Team 6");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1);
        e.setAway_goals(1);
        e.setCompetition_id("IT1");
        ml.add(e);

        try {
            mDAO.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        // 2/3 matches go "FINISHED".
        ml = new ArrayList<>();
        e = new Match();
        e.setMatchID(mDAO.getLastID() - 2);
        e.setStatus("FINISHED");
        e.setTeam_home("Test Team 1");
        e.setTeam_away("Test Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(1); // The 1° bet should have status = 1.
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        ml.add(e);

        e = new Match();
        e.setMatchID(mDAO.getLastID() - 1);
        e.setStatus("FINISHED");
        e.setTeam_home("Test Team 3");
        e.setTeam_away("Test Team 4");
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