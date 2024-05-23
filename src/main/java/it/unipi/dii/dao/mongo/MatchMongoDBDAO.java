package it.unipi.dii.dao.mongo;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import it.unipi.dii.dao.MatchDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import it.unipi.dii.dao.redis.SlipRedisDAO;
import it.unipi.dii.model.Match;
import it.unipi.dii.model.Slip;
import org.bson.Document;

import java.io.IOException;
import java.time.Instant;
import java.util.*;

import static it.unipi.dii.utility.dateTimes.*;
import static it.unipi.dii.utility.converters.jsonToDocumentConverter.convertDocumentToJson;
import static it.unipi.dii.utility.converters.jsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.mongoUtility.insertDocuments;
import static it.unipi.dii.utility.converters.objectToJsonStringConverter.convertObjectToJsonString;
import static it.unipi.dii.utility.sportAPI.getNewMatchesUpdates;

public class MatchMongoDBDAO extends BaseMongoDAO implements MatchDAO {
    /**
     * Insert a new match in the MongoDB database.
     * @param match The match object to add in the MongoDB collection.
     */
    @Override
    public void addMatch(Match match) {
        if (match.checkMatchValidity()) { // If the match object is valid.
            match.cleanGoals(); // Cleaning some input fields.
            match.setMatchID(this.getLastID() + 1); // Fetch the new matchID.
            MongoCollection<Document> match_coll = this.mongoDB.getCollection("matches");
            List<Document> documents = new ArrayList<>();
            documents.add(Document.parse(convertObjectToJsonString(match)));
            insertDocuments(match_coll, documents);
        }
    }

    /**
     * Delete matches from the database depending on the query.
     *
     * @param query Query for select the matches to delete.
     */

    @Override
    public void removeMatch(Document query) {
        MongoCollection<Document> match_coll = this.mongoDB.getCollection("matches");
        match_coll.findOneAndDelete(query);
    }

    /**
     * This function updates the match date field of a match document in MongoDB.
     *
     * @param matchID The target match ID.
     * @param newDate The new match date.
     */
    private void updateMatchDate(int matchID, String newDate) {
        Document filter = new Document("matchID", matchID);
        Document update = new Document("$set", new Document("matchDate", newDate).append("status", "TIMED"));
        this.mongoDB.getCollection("matches").updateOne(filter, update);
    }

    /**
     * This function updates some of the fields of a match document in MongoDB.
     *
     * @param matchID    The target match ID.
     * @param status     The new status of the match.
     * @param home_goals The new number of goals of the home team.
     * @param away_goals The new number of goals of the home team.
     */

    private void updateMatchStatusAndResult(int matchID, String status, int home_goals, int away_goals) {
        Document filter = new Document("matchID", matchID);
        Document update = new Document("$set",
                new Document("status", status)
                        .append("home_goals", home_goals)
                        .append("away_goals", away_goals)
        );
        this.mongoDB.getCollection("matches").updateOne(filter, update);
    }

    /**
     * @return The biggest value of MatchID from MongoDB.
     */

    @Override
    public Integer getLastID() {
        // Firsts let's try with a limit on the match date to speed up the query.
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("matchDate",
                                new Document("$gt", getCurrentDate().minusMonths(2).toString()))),
                new Document("$project",
                        new Document("matchID", 1L)
                                .append("_id", 0L)),
                new Document("$sort",
                        new Document("matchID", -1L)),
                new Document("$limit", 1L));
        AggregateIterable<Document> result = this.mongoDB.getCollection("matches").aggregate(pipeline);
        try {
            return Objects.requireNonNull(result.first()).getInteger("matchID");
        } catch (NullPointerException e1) {
            // Now we will try with no limit on the match date.
            // the query is slower, but it surely returns something.
            pipeline = Arrays.asList(new Document("$project",
                            new Document("matchID", 1L)
                                    .append("_id", 0L)),
                    new Document("$sort",
                            new Document("matchID", -1L)),
                    new Document("$limit", 1L));
            result = this.mongoDB.getCollection("matches").aggregate(pipeline);
            try {
                return Objects.requireNonNull(result.first()).getInteger("matchID");
            } catch (NullPointerException e2) {
                return -1; // The match collection is empty.
            }
        }
    }

    /**
     * Given a match object, the function return the ID related to the match specification (matchDate, home and away teams).
     * @param match The target match object.
     * @return An integer which is the ID of the match:
     * If the match object don't have the ID;
     * If the match object does not have an ID then a query in MongoDB is made;
     * If the match does not exist in MongoDB, -1 is returned.
     */
    @Override
    public Integer getID(Match match) {
        if (match.getMatchID() == null || match.getMatchID() == -1) {
            List<Match> ml = getMatches(getAndCondition(match, "date"), new Document("_id", 0));
            if (ml.isEmpty()) {
                return -1;
            } else {
                return ml.get(0).getMatchID();
            }
        } else {
            return match.getMatchID();
        }
    }

    /**
     * Short-cut function to build a match criteria.
     * @param targetMatch The target match.
     * @param mode Can be "date" or "competition".
     * @return A document for build a query match statement.
     */
    public Document getAndCondition(Match targetMatch, String mode) {
        List<Document> andConditions = new ArrayList<>();

        Document condition;

        if (Objects.equals(mode, "competition")) {
            condition = new Document("competition_id", new Document("$eq", targetMatch.getCompetition_id()));
        } else if (Objects.equals(mode, "date")) {
            condition = new Document("matchDate", new Document("$eq", targetMatch.getMatchDate()));
        } else {
            return new Document("$eq", targetMatch.getCompetition_id());
        }

        andConditions.add(condition);
        condition = new Document("team_home", new Document("$eq", targetMatch.getTeam_home()));
        andConditions.add(condition);
        condition = new Document("team_away", new Document("$eq", targetMatch.getTeam_away()));
        andConditions.add(condition);
        return new Document("$and", andConditions);
    }

    /**
     * Get new updates from a public API and update the matches in MongoDB.
     */
    public void updateMatches() throws IOException {
        this.updateMatches(getNewMatchesUpdates()); // Must be called every 60 seconds.
    }

    /**
     * Given a matches update list, applies the updates to the match collection.
     * @param ml The matches updates list.
     */

    public void updateMatches(List<Match> ml) throws IOException {
        final int size = ml.size(); // Size of the update list.
        if (size > 0) {
            SlipMongoDBDAO sDAO = new SlipMongoDBDAO();
            sDAO.openConnection();
            for (int i = 0; i < size; i++) {
                if (Objects.equals(ml.get(i).getStatus(), "TIMED")) {
                    // The match must be inserted in MongoDB.
                    if (matchAlreadyPresent(ml.get(i)) == -1) {
                        // If the match is not already present in the database.
                        addMatch(ml.get(i));
                    }
                } else if (Objects.equals(ml.get(i).getStatus(), "CANCELED")) {
                    //This match must be removed from all non-confirmed slips in Redis
                    SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
                    List<String> usernameList = slipRedisDAO.getAllUsernames(); // Taking all usernames from Redis.
                    // Take in a big slip list all the slip of users in redis
                    for (String username : usernameList) {
                        List<Slip> slipList = slipRedisDAO.getListFromUser(username);
                        for (Slip slip : slipList) {
                            if (slip.getSlipID() != null) {
                                // Check where match appears in the bet list of the slip.
                                for (int j = 0; j < slip.findBetsList().size(); j++) {
                                    if (Objects.equals(slip.findBetsList().get(j).getTeamAway(), ml.get(i).getTeam_away()) && Objects.equals(slip.findBetsList().get(j).getTeamHome(), ml.get(i).getTeam_home())) {
                                        if (slip.findBetsList().size() > 1) {
                                            slipRedisDAO.deleteBetFromSlip(username, slip.getSlipID(), slip.findBetsList().get(j));
                                        } else {
                                            // If it's the last one.
                                            slipRedisDAO.delete_Slip(username, slip.getSlipID());
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // The match must be canceled from MongoDB.
                    Integer id = matchAlreadyPresent(ml.get(i));
                    if (id > -1) {
                        // There is a match with those information.
                        sDAO.removeAllBetsOfMatch(id); // And we must remove all bets of this match.
                        removeMatch(getAndCondition(ml.get(i), "date")); // And delete the match.
                    }
                } else if (Objects.equals(ml.get(i).getStatus(), "POSTPONED")) {
                    // The match has been postponed.
                    List<Match> mlist = getMatches(getAndCondition(ml.get(i), "competition").append("status", "TIMED"), new Document("_id", 0));
                    if (mlist.isEmpty()) {
                        continue;
                    }
                    final Integer index = nearestMatch(ml.get(i), mlist);
                    updateMatchDate(mlist.get(index).getMatchID(), ml.get(i).getMatchDate());
                    sDAO.updateBetsMatchPostponed(mlist.get(index).getMatchID(), ml.get(i).getMatchDate());
                    // Make the Redis query.
                    SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
                    List<String> usernameList = slipRedisDAO.getAllUsernames(); // Taking all usernames from Redis.
                    // Take in a big slip list all the slip of users in redis
                    for (String username : usernameList) {
                        List<Slip> slipList = slipRedisDAO.getListFromUser(username);
                        for (Slip slip : slipList) {
                            if (slip.getSlipID() != null) {
                                // Check where match appears in the bet list of the slip.
                                for (int j = 0; j < slip.findBetsList().size(); j++) {
                                    if (Objects.equals(slip.findBetsList().get(j).getTeamAway(), ml.get(i).getTeam_away()) && Objects.equals(slip.findBetsList().get(j).getTeamHome(), ml.get(i).getTeam_home())) {
                                        if (slip.findBetsList().size() > 1) {
                                            slipRedisDAO.deleteBetFromSlip(username, slip.getSlipID(), slip.findBetsList().get(j));
                                        } else {
                                            slipRedisDAO.delete_Slip(username, slip.getSlipID()); // If it's the last one.
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else if (Objects.equals(ml.get(i).getStatus(), "FINISHED")) {
                    // The match is finished.
                    final Integer id = getID(ml.get(i));
                    updateMatchStatusAndResult(id, "FINISHED", ml.get(i).getHome_goals(), ml.get(i).getAway_goals());
                    if (id >= 0) {
                        sDAO.checkSlipsWhenMatchEnds(id);
                    }
                } else if (Objects.equals(ml.get(i).getStatus(), "IN_PLAY") || Objects.equals(ml.get(i).getStatus(), "PAUSED")) {
                    // Update of the match In MongoDB, because the match is started, but is not finished yet

                    List<Match> mlist = this.getMatches(
                            getAndCondition(ml.get(i), "date"),
                            new Document("_id", 0)
                    );

                    if (mlist.isEmpty()) {
                        continue;
                    }

                    for (Match m : mlist) {
                        if (m != null) {
                            if (Objects.equals(m.getStatus(), "TIMED")) {
                                // If it was timed I have to delete all non-confirmed slips in redis for consistency issues.
                                SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
                                List<String> usernameList = slipRedisDAO.getAllUsernames(); // Taking all usernames from Redis.

                                // Take in a big slip list all the slip of users in redis
                                for (String username : usernameList) {
                                    List<Slip> slipList = slipRedisDAO.getListFromUser(username);
                                    for (Slip slip : slipList) {
                                        if (slip.getSlipID() != null) {
                                            // Check where match appears in the bet list of the slip.
                                            for (int j = 0; j < slip.findBetsList().size(); j++) {
                                                if (Objects.equals(slip.findBetsList().get(j).getTeamAway(), ml.get(i).getTeam_away()) && Objects.equals(slip.findBetsList().get(j).getTeamHome(), ml.get(i).getTeam_home())) {
                                                    if (slip.findBetsList().size() > 1) {
                                                        slipRedisDAO.deleteBetFromSlip(username, slip.getSlipID(), slip.findBetsList().get(j));
                                                    } else {
                                                        // If it's the last one.
                                                        slipRedisDAO.delete_Slip(username, slip.getSlipID());
                                                    }

                                                }
                                            }
                                        }
                                    }
                                }

                            }
                        }
                    }
                    final Integer id = getID(ml.get(i));
                    updateMatchStatusAndResult(id, ml.get(i).getStatus(), ml.get(i).getHome_goals(), ml.get(i).getAway_goals());
                }
            }
            sDAO.closeConnection();
        }
    }

    /**
     * Given a target match and a list of matches, the function find the nearest (in time terms) match to the target one.
     * @param m  The target match object.
     * @param ml The list of matches.
     * @return The index of the match in the list with the nearest matchDate of the target match.
     */

    private Integer nearestMatch(Match m, List<Match> ml) {
        final Instant dateMatch = stringToTimestamp(m.getMatchDate());
        Instant dateIter = stringToTimestamp(ml.get(0).getMatchDate());
        assert dateMatch != null;
        assert dateIter != null;

        long minDays = differenceSeconds(dateMatch, dateIter), days;
        int minIndex = 0;
        for (int i = 1; i < ml.size(); i++) {
            if (!Objects.equals(ml.get(i).getStatus(), "TIMED")) {
                continue;
            }

            days = differenceSeconds(
                    dateMatch,
                    stringToTimestamp(ml.get(i).getMatchDate())
            );

            if (days < minDays) {
                minDays = days;
                minIndex = i;
            }
        }
        return minIndex;
    }

    /**
     * Check if the target match information are already been inserted in MongoDB.
     * @param match The target match object.
     * @return The matchID if a match with same date and same teams already is present in the database; -1 instead.
     */

    private Integer matchAlreadyPresent(Match match) {
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("$and", Arrays.asList(new Document("matchDate", match.getMatchDate()),
                                new Document("team_home", match.getTeam_home()),
                                new Document("team_away", match.getTeam_away())))),
                new Document("$project",
                        new Document("matchID", 1L)
                                .append("_id", 0L)));

        AggregateIterable<Document> docs = this.mongoDB.getCollection("matches").aggregate(pipeline);
        try {
            return Objects.requireNonNull(docs.first()).getInteger("matchID");
        } catch (NullPointerException e) {
            return -1;
        }
    }

    /**
     * Given match criteria, the function returns the list of matches that match the criteria.
     * @param query      Match criteria of the matches.
     * @param projection Attribute to project.
     * @return The list of match that respect the criteria.
     */

    @Override
    public List<Match> getMatches(Document query, Document projection) {
        MongoCollection<Document> match_coll = this.mongoDB.getCollection("matches");
        List<Match> s_list = new ArrayList<>();
        try (MongoCursor<Document> cursor = match_coll.find(query).projection(projection).iterator()) {
            while (cursor.hasNext()) {
                Document document = cursor.next();
                s_list.add(convertJsonToObject(convertDocumentToJson(document), Match.class));
            }
        }
        return s_list;
    }

    /**
     * Given a matchID, the function return a match object from MongoDB.
     * @param matchID The target match id.
     * @return The related match object, or null if a match with that ID does not exist.
     */

    @Override
    public Match getMatch(Integer matchID) {
        List<Match> ml = getMatches(new Document("matchID", new Document("$eq", matchID)), new Document("_id", 0));
        if (ml.isEmpty()) {
            return null;
        } else {
            return ml.get(0);
        }
    }
}
