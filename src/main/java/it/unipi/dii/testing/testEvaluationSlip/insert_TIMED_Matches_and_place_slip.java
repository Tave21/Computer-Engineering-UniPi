package it.unipi.dii.testing.testEvaluationSlip;

import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.dao.mongo.SlipMongoDBDAO;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Match;
import it.unipi.dii.model.Slip;

import java.io.IOException;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.dateTimes.getCurrentInstant;
import static it.unipi.dii.utility.mongoUtility.deactivateMongoDBNotifications;

public class insert_TIMED_Matches_and_place_slip {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();
        MatchMongoDBDAO mDAO = new MatchMongoDBDAO();
        List<Match> ml = new ArrayList<>();

        final String thisInstant = getCurrentInstant().plus(2 , ChronoUnit.DAYS).toString();

        mDAO.openConnection();
        Match e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Test Manual Timed Team 1");
        e.setTeam_away("Test Manual Timed Team 2");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("IT1");
        e.initializeAndRandomizeMultipliers();
        ml.add(e);

        e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Test Manual Timed Team 3");
        e.setTeam_away("Test Manual Timed Team 4");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("GB1");
        e.initializeAndRandomizeMultipliers();
        ml.add(e);

        e = new Match();
        e.setStatus("TIMED");
        e.setTeam_home("Test Manual Timed Team 5");
        e.setTeam_away("Test Manual Timed Team 6");
        e.setMatchDate(thisInstant);
        e.setHome_goals(0);
        e.setAway_goals(0);
        e.setCompetition_id("FR1");
        e.initializeAndRandomizeMultipliers();
        ml.add(e);

        try {
            mDAO.updateMatches(ml);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }


        SlipMongoDBDAO sDAO = new SlipMongoDBDAO();
        sDAO.openConnection();

        // Create a new slip on those 3 matches.

        Slip s = new Slip();
        s.setUsername("user");
        s.setBetAmount(10);
        List<Bet> betList = new ArrayList<>();

        for(int i = 0 ; i < ml.size() ; i++){
            Bet b = new Bet(
                    mDAO.getLastID() - i,
                    ml.get(i).pickMultiplierValue(i),
                    ml.get(i).pickMultiplierName(i), // 1 , 2 , X
                    ml.get(i).getMatchDate()
            );
            b.setTeamHome(ml.get(i).getTeam_home());
            b.setTeamAway(ml.get(i).getTeam_away());
            b.setCompetition_id(ml.get(i).getCompetition_id());
            betList.add(b);
        }
        s.setBetsList(betList);
        s.setCreationDate(getCurrentInstant().minusSeconds(3600).toString());
        s.setConfirmationDate(getCurrentInstant().minusSeconds(10).toString());
        sDAO.addSlip(s);
        sDAO.closeConnection();
        mDAO.closeConnection();
    }
}
