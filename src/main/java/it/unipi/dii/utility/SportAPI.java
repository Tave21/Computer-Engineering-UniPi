package it.unipi.dii.utility;

import it.unipi.dii.model.Match;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;

import static it.unipi.dii.utility.DateTimes.*;

public class SportAPI {
    public static final String API_KEY = "d78f2f22b4324475b801e9fe8534eba7"; // Key to access to the services of the API.

    public SportAPI() {
    }

    /**
     * @return The list of matches updates of the 5 european competitions (Italy, Germany, France, UK and Spain).
     */
    public static List<Match> getNewMatchesUpdates() {
        List<Match> a = getLeague("SA");
        List<Match> ret = new ArrayList<>(a);
        a = getLeague("PL");
        ret.addAll(a);
        a = getLeague("PD");
        ret.addAll(a);
        a = getLeague("FL1");
        ret.addAll(a);
        a = getLeague("BL1");
        ret.addAll(a);
        return ret;
    }

    /**
     * Returns a list of Match object. created with data gathered from the API.
     *
     * @param leagueName Name of the football competition.
     * @return The list of matches updates of the 5 european competitions (Italy, Germany, France, UK and Spain).
     */

    public static List<Match> getLeague(String leagueName) {
        LocalDate dateFrom = getCurrentDate().minusDays(1);
        LocalDate dateTo = dateFrom.plusDays(15);
        return getApiContent("https://api.football-data.org/v4/competitions/" + leagueName + "/matches/?dateFrom=" + dateFrom + "&dateTo=" + dateTo, leagueName);
    }

    /**
     * Returns a list of Match object. created with data gathered from the API.
     * The url argument must specify an absolute <a href="#{@link}">{@link URL}</a>. The name
     * argument is a specifier that is relative to the url argument.
     * <br>
     * This method always returns immediately, whether there are new updates or not.
     * <br>
     * Available competitions: <br>
     * PL -> Premier League <br>
     * SA -> Serie A <br>
     * PD -> Primera División <br>
     * BL1 -> Bundesliga <br>
     * FL1 -> League 1 <br>
     * PPL -> Prairie Ligand <br>
     * ELC -> Championship <br>
     * DED -> Eredivisie <br>
     * CL -> Champions League <br>
     * EC -> European Championship <br>
     * WC -> World Cup <br>
     * BSA -> Brasileirao Serie A <br>
     * CLI -> Copa Libertadores <br>
     * <br>
     * Possible match status are: <br>
     * Scheduled --> The match date has been decided but the date is NOT sure. <br>
     * Timed --> The match date has been decided and the date is sure. <br>
     * in_play --> The match is on going. <br>
     * Paused --> The match begins, but now there is a break. <br>
     * Finished --> The match is finished. <br>
     * Canceled --> The match has been canceled. <br>
     * Postponed --> The match has been postponed. <br>
     * Suspended --> temporally suspended. <br>
     * Awarded --> The match has finished before the begins. <br>
     *
     * @param apiUrl     URL of the resource that you want to get,
     * @param leagueName Name of the football competition.
     * @return The list of matches updates.
     */
    private static List<Match> getApiContent(String apiUrl, String leagueName) {
        try {
            URL url = new URL(apiUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setRequestProperty("X-Auth-Token", API_KEY);

            InputStream inputStream = connection.getInputStream();

            String jsonResponse = new Scanner(inputStream, StandardCharsets.UTF_8).useDelimiter("\\A").next();

            JSONObject jsonObject = new JSONObject(jsonResponse);
            JSONArray matches = jsonObject.getJSONArray("matches");
            List<Match> ret = new ArrayList<>();

            for (int i = 0; i < matches.length(); i++) {

                JSONObject match = matches.getJSONObject(i);

                Instant lastUpdated = stringToTimestamp(match.getString("lastUpdated"));

                if (
                        lastUpdated != null &&
                                !Objects.equals(lastUpdated.toString(), "null") &&
                                !Objects.equals(lastUpdated.toString(), "NULL")
                ) {
                    // lastUpdated.compareTo(getCurrentInstant().minusSeconds(65)) > 0
                    if (true) {
                        // If the update is "new enough".
                        JSONObject score = match.getJSONObject("score");
                        JSONObject fullTime = score.optJSONObject("fullTime"); // final result.
                        Match m = new Match();
                        m.setMatchID(-1);
                        m.setMatchDate(match.getString("utcDate"));
                        m.setTeam_home(match.getJSONObject("homeTeam").getString("name"));
                        m.setTeam_away(match.getJSONObject("awayTeam").getString("name"));

                        if (fullTime.get("home") == null || Objects.equals(fullTime.get("home").toString(), "null")) {
                            m.setHome_goals(null);
                        } else {
                            m.setHome_goals(Integer.valueOf(fullTime.get("home").toString()));
                        }

                        if (fullTime.get("away") == null || Objects.equals(fullTime.get("away").toString(), "null")) {
                            m.setAway_goals(null);
                        } else {
                            m.setAway_goals(Integer.valueOf(fullTime.get("away").toString()));
                        }

                        m.setStatus(match.getString("status"));

                        switch (leagueName) {
                            case "SA":
                                m.setCompetition_id("IT1");
                                break;
                            case "PL":
                                m.setCompetition_id("GB1");
                                break;
                            case "PD":
                                m.setCompetition_id("ES1");
                                break;
                            case "FL1":
                                m.setCompetition_id("FR1");
                                break;
                            case "BL1":
                                m.setCompetition_id("L1");
                                break;
                            default:
                                m.setCompetition_id("null");
                                break;
                        }

                        if (!Objects.equals(m.getCompetition_id(), "null")) {
                            ret.add(m);
                            // ISODate('1852-01-15T11:25:00Z'),
                            // "2018-10-28T23:58:18Z"
                        }

                    }
                }
            }

            /*
            Example of the object returned by the API:

                {"area":
                {"code":"NLD","flag":"https://crests.football-data.org/8601.svg","name":"Netherlands","id":2163}
                ,"matchday":18
                ,"awayTeam":{"name":"Fortuna Sittard","tla":"SIT","id":1920,"shortName":"Sittard","crest":"https://crests.football-data.org/1920.png"}
                ,"competition":{"code":"DED","name":"Eredivisie","id":2003,"type":"LEAGUE","emblem":"https://crests.football-data.org/ED.png"}
                ,"utcDate":"2024-01-24T17:45:00Z"
                ,"lastUpdated":"2024-01-22T20:21:04Z"
                ,"score":{"duration":"REGULAR","winner":"DRAW","halfTime":{"away":null,"home":null}
                ,"fullTime":{"away":0,"home":0}}
                ,"stage":"REGULAR_SEASON"
                ,"odds":{"msg":"Activate Odds-Package in User-Panel to retrieve odds."}
                ,"season":{"winner":null,"currentMatchday":18,"endDate":"2024-05-19","id":1590,"startDate":"2023-08-11"}
                ,"homeTeam":{"name":"Almere City FC","tla":"ALM","id":1911,"shortName":"Almere City","crest":"https://crests.football-data.org/1911.png"}
                ,"id":441639
                ,"referees":[{"nationality":"Netherlands","name":"Jeroen Manschot","id":56912,"type":"REFEREE"}]
                ,"status":"TIMED"
                ,"group":null
            */

            connection.disconnect();
            return ret;

        } catch (IOException ignored) {

        }
        return null;
    }

}