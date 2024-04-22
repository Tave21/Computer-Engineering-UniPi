package it.unipi.dii.model;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Objects;

public class Bet {
    private Integer matchID;
    private String teamHome;
    private String teamAway;
    private String chosenMultiplierName;
    private double chosenMultiplierValue;
    private String competition_id;
    private String matchDate;
    private Integer win;

    public Bet(Integer matchID, double chosenMultiplierValue , String chosenMultiplierName , String matchDate) {
        this.matchID = matchID;
        this.chosenMultiplierValue = chosenMultiplierValue;
        this.chosenMultiplierName = chosenMultiplierName;
        this.matchDate = matchDate;
        this.win = -1;
    }

    public Bet(){
        this.win = -1;
    }

    @Override
    public boolean equals(Object bet) {
        if (this == bet) {
            return true;
        }
        if (bet == null || getClass() != bet.getClass()) {
            return false;
        }
        Bet otherBet = (Bet) bet;
        return Objects.equals(matchID, otherBet.matchID) &&
                Objects.equals(teamHome, otherBet.teamHome) &&
                Objects.equals(teamAway, otherBet.teamAway) &&
                Objects.equals(chosenMultiplierName, otherBet.chosenMultiplierName) &&
                Double.compare(otherBet.chosenMultiplierValue, chosenMultiplierValue) == 0 &&
                Objects.equals(competition_id, otherBet.competition_id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(matchID, teamHome, teamAway, chosenMultiplierName, chosenMultiplierValue, competition_id);
    }

    @JsonProperty("matchID")
    public Integer getMatchID() {
        return matchID;
    }
    @JsonProperty("win")
    public Integer getWin(){ return this.win;}
    @JsonProperty("chosenMultipliervalue")
    public double getChosenMultiplierValue() {
        return this.chosenMultiplierValue;
    }
    @JsonProperty("chosenMultiplierName")
    public String getChosenMultiplierName() {
        return this.chosenMultiplierName;
    }
    @JsonProperty("competition_id")
    public String getCompetitionId() {
        return this.competition_id;
    }
    @JsonProperty("teamHome")
    public String getTeamHome() {
        return this.teamHome;
    }
    @JsonProperty("teamAway")
    public String getTeamAway() {
        return this.teamAway;
    }
    @JsonProperty("matchDate")
    public String getMatchDate() {
        return this.matchDate;
    }

    public void setMatchID(Integer matchID) {
        this.matchID = matchID;
    }

    public void setChosenMultiplierValue(double chosenMultiplierValue) {
        this.chosenMultiplierValue = chosenMultiplierValue;
    }

    public void setCompetition_id(String competition_id) {
        this.competition_id = competition_id;
    }

    public void setTeamHome(String teamHome) {
        this.teamHome = teamHome;
    }

    public void setMatchDate(String matchDate) {
        this.matchDate = matchDate;
    }

    public void setTeamAway(String teamAway) {
        this.teamAway = teamAway;
    }
    public void setChosenMultiplierName(String chosenMultiplierName) {
        this.chosenMultiplierName = chosenMultiplierName;
    }
    public void setWin(Integer win){
        if (win == 1 || win == 0 || win == -1) {
            // Only accepted values.
            this.win = win;
        }
    }
    public void setWin(boolean win){
        if(win){
            this.win = 1;
        }else{
            this.win = 0;
        }
    }

    @Override
    public String toString() {
        return "Bet{" +
                "matchID=" + matchID+
                ", teamHome=" + teamHome+
                ", teamAway=" + teamAway+
                ", chosenmultiplieValue=" + chosenMultiplierValue+
                ", chosenmultiplieName=" + chosenMultiplierName+
                ", competition_id=" + competition_id+
                ", matchDate=" + matchDate+
                ", win=" + win+
                '}';
    }
}
