package it.unipi.dii.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.randomGeneration.generateMultiplier;
import static it.unipi.dii.utility.regularExpressionChecks.isNaN;

public class Match {
    private Integer matchID;
    private String competition_id;
    private String team_home;
    private String team_away;
    private String matchDate;
    private Integer home_goals;
    private Integer away_goals;
    private String status;
    public List<Multiplier> multipliers = new ArrayList<>();
    private final Integer numberOfMultipliers = 23;

    public Match() {
        if(multipliers.isEmpty()) {
            this.initializeMultipliers();
            this.randomizeMultipliers();
        }
    }

    public Match(Integer matchID, String competition_id, String team_home, String team_away, String matchDate,
                 Integer home_goals, Integer away_goals) {
        this.matchID = matchID;
        this.competition_id = competition_id;
        this.team_home = team_home;
        this.team_away = team_away;
        this.matchDate = matchDate;
        this.home_goals = home_goals;
        this.away_goals = away_goals;
        this.cleanGoals();
        if(multipliers.isEmpty()) {
            this.initializeMultipliers();
            this.randomizeMultipliers();
        }
    }

    @JsonProperty("matchID")
    public Integer getMatchID() {
        return this.matchID;
    }

    public void setMatchID(Integer matchID) {
        this.matchID = matchID;
    }

    @JsonProperty("competition_id")
    public String getCompetition_id() {
        return competition_id;
    }

    public void setCompetition_id(String competition_id) {
        this.competition_id = competition_id;
    }

    @JsonProperty("team_home")
    public String getTeam_home() {
        return team_home;
    }

    public void setTeam_home(String team_home) {
        this.team_home = team_home;
    }

    @JsonProperty("team_away")
    public String getTeam_away() {
        return team_away;
    }

    public void setTeam_away(String team_away) {
        this.team_away = team_away;
    }

    @JsonProperty("matchDate")
    public String getMatchDate() {
        return matchDate;
    }

    public void setMatchDate(String matchDate) {
        this.matchDate = matchDate;
    }

    @JsonProperty("multipliers")
    public List<Multiplier> getMultipliers() {
        return this.multipliers;
    }
    public void setMultipliers(List<Multiplier> multipliers) {
        this.multipliers = multipliers;
    }
    public double pickMultiplierValue(int index) {
        return this.multipliers.get(index).getValue();
    }

    public String pickMultiplierName(int index) {
        return this.multipliers.get(index).getName();
    }

    public void setMultiplier(int index, String name, double value) {
        this.multipliers.set(index, new Multiplier(name, value));
    }

    @JsonProperty("home_goals")
    public Integer getHome_goals() {
        return home_goals;
    }

    public void setHome_goals(Integer home_goals) {
        this.home_goals = home_goals;
    }

    @JsonProperty("away_goals")
    public Integer getAway_goals() {
        return away_goals;
    }

    public void setAway_goals(Integer away_goals) {
        this.away_goals = away_goals;
    }

    @JsonProperty("status")
    public String getStatus() {
        return this.status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    /**
     * Check if the match meets the requirements of the target multiplier.
     * The list of the supported multipliers:
     * <ul>
     *     <li>"1" - Home team wins.</li>
     *     <li>"2" - Away team wins.</li>
     *     <li>"X" - Draw.</li>
     *     <li>"1X" - Home team wins or draw.</li>
     *     <li>"X2" - Away team wins or draw.</li>
     *     <li>"12" - No draw.</li>
     *     <li>"1G" - Home team wins but both teams score.</li>
     *     <li>"2G" - Away team wins but both teams score.</li>
     *     <li>"XG" - Draw but both teams score.</li>
     *     <li>"GG" - Both teams score.</li>
     *     <li>"NG" - No one scores.</li>
     *     <li>"Over0/5" - The match will end with at least 1 goal.</li>
     *     <li>"Over1/5" - The match will end with at least 2 goals.</li>
     *     <li>"Over2/5" - The match will end with at least 3 goals.</li>
     *     <li>"Over3/5" - The match will end with at least 4 goals.</li>
     *     <li>"Over4/5" - The match will end with at least 5 goals.</li>
     *     <li>"Over5/5" - The match will end with at least 6 goals.</li>
     *     <li>"Under0/5" - The match will end with a maximum of 0 goals.</li>
     *     <li>"Under1/5" - The match will end with a maximum of 1 goal.</li>
     *     <li>"Under2/5" - The match will end with a maximum of 2 goals.</li>
     *     <li>"Under3/5" - The match will end with a maximum of 3 goals.</li>
     *     <li>"Under4/5" - The match will end with a maximum of 4 goals.</li>
     *     <li>"Under5/5" - The match will end with a maximum of 5 goals.</li>
     * </ul>
     *
     * @param multiplierName The caption of the multiplier to check.
     * @return True if the multiplier condition are met by the match,
     * false otherwise or in the case that the multiplier caption does not exist.
     */
    public boolean checkMultiplierWin(String multiplierName) {
        this.cleanGoals();
        boolean res = false;
        switch (multiplierName) {
            case "1":
                if (this.home_goals > this.away_goals) {
                    res = true;
                }
                break;
            case "2":
                if (this.home_goals < this.away_goals) {
                    res = true;
                }
                break;
            case "X":
                if (this.home_goals == this.away_goals) {
                    res = true;
                }
                break;
            case "1X":
                if (this.home_goals >= this.away_goals) {
                    res = true;
                }
                break;
            case "X2":
                if (this.home_goals <= this.away_goals) {
                    res = true;
                }
                break;
            case "12":
                if (!Objects.equals(this.home_goals, this.away_goals)) {
                    res = true;
                }
                break;
            case "1G":
                if ((this.home_goals > this.away_goals) && (this.away_goals > 0)) {
                    res = true;
                }
                break;
            case "2G":
                if ((this.home_goals < this.away_goals) && (this.home_goals > 0)) {
                    res = true;
                }
                break;
            case "XG":
                if ((Objects.equals(this.home_goals, this.away_goals)) && (this.home_goals > 0)) {
                    res = true;
                }
                break;
            case "GG":
                if ((this.away_goals > 0) && (this.home_goals > 0)) {
                    res = true;
                }
                break;
            case "NG":
                if ((this.away_goals == 0) && (this.home_goals == 0)) {
                    res = true;
                }
                break;
            case "Over0/5":
                if (this.away_goals + this.home_goals > 0) {
                    res = true;
                }
                break;
            case "Over1/5":
                if (this.away_goals + this.home_goals > 1) {
                    res = true;
                }
                break;
            case "Over2/5":
                if (this.away_goals + this.home_goals > 2) {
                    res = true;
                }
                break;
            case "Over3/5":
                if (this.away_goals + this.home_goals > 3) {
                    res = true;
                }
                break;
            case "Over4/5":
                if (this.away_goals + this.home_goals > 4) {
                    res = true;
                }
                break;
            case "Over5/5":
                if (this.away_goals + this.home_goals > 5) {
                    res = true;
                }
                break;
            case "Under0/5":
                if (this.away_goals + this.home_goals == 0) {
                    res = true;
                }
                break;
            case "Under1/5":
                if (this.away_goals + this.home_goals < 2) {
                    res = true;
                }
                break;
            case "Under2/5":
                if (this.away_goals + this.home_goals < 3) {
                    res = true;
                }
                break;
            case "Under3/5":
                if (this.away_goals + this.home_goals < 4) {
                    res = true;
                }
                break;
            case "Under4/5":
                if (this.away_goals + this.home_goals < 5) {
                    res = true;
                }
                break;
            case "Under5/5":
                if (this.away_goals + this.home_goals < 6) {
                    res = true;
                }
                break;
            default:
        }
        return res;
    }

    /**
     * If away_goals or home_goals are set to NULL, this function set them to 0.
     */
    public void cleanGoals() {
        if (this.home_goals == null || isNaN((double)this.home_goals)) {
            this.home_goals = 0;
        }
        if (this.away_goals == null || isNaN((double)this.away_goals)) {
            this.away_goals = 0;
        }
    }

    /**
     * Initialize the list of the multipliers of a Match object.
     * Multipliers values are randomly generated.
     */

    private void initializeMultipliers() {
        if(this.multipliers.isEmpty()) {
            for (int i = 0; i < this.numberOfMultipliers; i++) {
                this.multipliers.add(new Multiplier("-", 0));
            }
        }
    }

    /**
     * Set the multipliers name to the canonical ones and randomize their values:
     * The list of the supported multipliers:
     *  <ul>
     *     <li>"1" - Home team wins.</li>
     *      <li>"2" - Away team wins.</li>
     *      <li>"X" - Draw.</li>
     *     <li>"1X" - Home team wins or draw.</li>
     *      <li>"X2" - Away team wins or draw.</li>
     *      <li>"12" - No draw.</li>
     *      <li>"1G" - Home team wins but both teams score.</li>
     *      <li>"2G" - Away team wins but both teams score.</li>
     *      <li>"XG" - Draw but both teams score.</li>
     *      <li>"GG" - Both teams score.</li>
     *     <li>"NG" - No one scores.</li>
     *     <li>"Over0/5" - The match will end with at least 1 goal.</li>
     *      <li>"Over1/5" - The match will end with at least 2 goals.</li>
     *      <li>"Over2/5" - The match will end with at least 3 goals.</li>
     *      <li>"Over3/5" - The match will end with at least 4 goals.</li>
     *      <li>"Over4/5" - The match will end with at least 5 goals.</li>
     *      <li>"Over5/5" - The match will end with at least 6 goals.</li>
     *      <li>"Under0/5" - The match will end with a maximum of 0 goals.</li>
     *      <li>"Under1/5" - The match will end with a maximum of 1 goal.</li>
     *      <li>"Under2/5" - The match will end with a maximum of 2 goals.</li>
     *      <li>"Under3/5" - The match will end with a maximum of 3 goals.</li>
     *      <li>"Under4/5" - The match will end with a maximum of 4 goals.</li>
     *      <li>"Under5/5" - The match will end with a maximum of 5 goals.</li>
     *  </ul>
     */
    public void randomizeMultipliers() {
        if(!this.multipliers.isEmpty() && Objects.equals(this.multipliers.get(0).getName(), "-")) {
            double temp = 0;
            this.setMultiplier(0, "1", generateMultiplier(3));
            this.setMultiplier(1, "2", generateMultiplier(3));
            this.setMultiplier(2, "X", generateMultiplier(3));
            this.setMultiplier(3, "1X", generateMultiplier(2));
            this.setMultiplier(4, "X2", generateMultiplier(2));
            this.setMultiplier(5, "12", generateMultiplier(2));

            this.setMultiplier(6, "1G", generateMultiplier(2));
            this.setMultiplier(7, "2G", generateMultiplier(3));
            this.setMultiplier(8, "GG", generateMultiplier(2));
            this.setMultiplier(9, "XG", generateMultiplier(3));
            this.setMultiplier(10, "NG", generateMultiplier(2));

            temp = 0;
            temp = temp + generateMultiplier(2);
            this.setMultiplier(11, "Over0/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(12, "Over1/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(13, "Over2/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(14, "Over3/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(15, "Over4/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(16, "Over5/5", temp);

            temp = 0;
            temp = temp + generateMultiplier(2);
            this.setMultiplier(22, "Under5/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(21, "Under4/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(20, "Under3/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(19, "Under2/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(18, "Under1/5", temp);
            temp = temp + generateMultiplier(2);
            this.setMultiplier(17, "Under0/5", temp);
        }
    }

    /**
     * create a string with all attributes of the Matches
     * @return a string in a JSON format
     */
    @Override
    public String toString() {
        return "Match{" +
                "matchID=" + matchID +
                ", competition_id='" + competition_id + '\'' +
                ", team_home='" + team_home + '\'' +
                ", team_away='" + team_away + '\'' +
                ", matchDate='" + matchDate + '\'' +
                ", home_goals=" + home_goals +
                ", away_goals=" + away_goals +
                ", status='" + status + '\'' +
                ", multipliers=" + multipliers +
                ", numberOfMultipliers=" + numberOfMultipliers +
                '}';
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;
        Match match = (Match) obj;
        return Objects.equals(matchID, match.matchID) &&
                Objects.equals(competition_id, match.competition_id) &&
                Objects.equals(team_home, match.team_home) &&
                Objects.equals(team_away, match.team_away) &&
                Objects.equals(matchDate, match.matchDate) &&
                Objects.equals(home_goals, match.home_goals) &&
                Objects.equals(away_goals, match.away_goals) &&
                Objects.equals(status, match.status) &&
                Objects.equals(multipliers, match.multipliers) &&
                Objects.equals(numberOfMultipliers, match.numberOfMultipliers);
    }
    /**
     * create a hash for the match
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return Objects.hash(matchID, competition_id, team_home, team_away, matchDate, home_goals, away_goals, status, multipliers, numberOfMultipliers);
    }
}