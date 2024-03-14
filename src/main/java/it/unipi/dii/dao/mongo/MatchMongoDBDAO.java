package it.unipi.dii.dao.mongo;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.result.UpdateResult;
import it.unipi.dii.dao.MatchDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import it.unipi.dii.dao.redis.SlipRedisDAO;
import it.unipi.dii.model.Match;
import it.unipi.dii.model.Slip;
import org.bson.Document;

import java.io.IOException;
import java.time.Instant;
import java.util.*;

import static it.unipi.dii.utility.DateTimes.*;
import static it.unipi.dii.utility.JsonToDocument.convertDocumentToJson;
import static it.unipi.dii.utility.JsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.ObjectToDocument.*;
import static it.unipi.dii.utility.MongoUtility.insertDocuments;
import static it.unipi.dii.utility.ObjectToJsonString.convertObjectToJsonString;
import static it.unipi.dii.utility.SportAPI.getNewMatchesUpdates;


public class MatchMongoDBDAO extends BaseMongoDAO implements MatchDAO {
    /**
     * Insert a new match in the database.
     *
     * @param match The match to add
     */
    @Override
    public void addMatch(Match match) {
        match.setMatchID(this.getLastID() + 1);
        MongoCollection<Document> match_coll = this.mongoDB.getCollection("matches");
        List<Document> documents = new ArrayList<>();
        documents.add(Document.parse(convertObjectToJsonString(match)));
        insertDocuments(match_coll, documents);
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
     * @param newMatch The new match to add.
     * @param mode     If true, the match is inserted even if the match is not into the database, otherwise the replacement is done only if a related match is in the database.
     * @return The id of the replaced match.
     */

    private Integer replaceMatch(Match newMatch, boolean mode) {
        MongoCollection<Document> match_coll = this.mongoDB.getCollection("matches");
        if (mode) {
            match_coll.deleteOne(new Document("matchID", new Document("$eq", newMatch.getMatchID())));
            match_coll.insertOne(ObjectToDocumentConverter(newMatch));
            return newMatch.getMatchID();
        } else {
            Integer id = getID(newMatch);
            if (id >= 0) {
                newMatch.setMatchID(id);
                match_coll.replaceOne(getAndCondition(newMatch, "date"), ObjectToDocumentConverter(newMatch));
            }
            return id;
        }
    }

    private void updateMatchDate(int matchID, String newDate) {
        Document filter = new Document("matchID", matchID);
        Document update = new Document("$set", new Document("matchDate", newDate).append("status", "POSTPONED"));
        this.mongoDB.getCollection("matches").updateOne(filter, update);
    }

    /**
     * @return The biggest value of MatchID from MongoDB.
     */

    @Override
    public Integer getLastID() {
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("matchDate",
                                new Document("$gt", getCurrentDate().minusMonths(24).toString()))),
                new Document("$project",
                        new Document("matchID", 1L)
                                .append("_id", 0L)),
                new Document("$sort",
                        new Document("matchID", -1L)),
                new Document("$limit", 1L));
        AggregateIterable<Document> result = this.mongoDB.getCollection("matches").aggregate(pipeline);
        try {
            return Objects.requireNonNull(result.first()).getInteger("matchID");
        } catch (NullPointerException e) {
            return -1;
        }
    }

    /**
     * @param match The target match.
     * @return The id of the match, if the match m don't have the id, then a query is made.
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

    public Document getAndCondition(Match m, String mode) {
        List<Document> andConditions = new ArrayList<>();

        Document condition;

        if (Objects.equals(mode, "competition")) {
            condition = new Document("competition_id", new Document("$eq", m.getCompetition_id()));
        } else if (Objects.equals(mode, "date")) {
            condition = new Document("matchDate", new Document("$eq", m.getMatchDate()));
        } else {
            return new Document("$eq", m.getCompetition_id());
        }

        andConditions.add(condition);
        condition = new Document("team_home", new Document("$eq", m.getTeam_home()));
        andConditions.add(condition);
        condition = new Document("team_away", new Document("$eq", m.getTeam_away()));
        andConditions.add(condition);
        return new Document("$and", andConditions);
    }

    /**
     * Get new updates from the internet and update the matches in MongoDB.
     */
    public void updateMatches() throws IOException {
        this.updateMatches(getNewMatchesUpdates()); // Must be called every 60 seconds.
    }

    public void updateMatches(List<Match> ml) throws IOException {
        final int size = ml.size();
        if (size > 0) {
            SlipMongoDBDAO sDAO = new SlipMongoDBDAO();
            sDAO.openConnection();

            for (int i = 0; i < size; i++) {
                if (Objects.equals(ml.get(i).getStatus(), "TIMED")) {
                    // The match must be inserted in MongoDB.
                    if (matchAlreadyPresent(ml.get(i)) == -1) {
                        // If the match is not already present in the database.
                        ml.get(i).randomizeMultipliers();
                        addMatch(ml.get(i));
                    }
                } else if (Objects.equals(ml.get(i).getStatus(), "CANCELED")) {
                    // The match must be canceled from MongoDB.
                    Integer id = matchAlreadyPresent(ml.get(i));
                    if (id > -1) {
                        // There is a match with those information.
                        sDAO.removeAllBetsOfMatch(id); // And we must remove all bets of this match.
                        removeMatch(getAndCondition(ml.get(i), "date")); // And delete the match.
                    }
                } else if (Objects.equals(ml.get(i).getStatus(), "POSTPONED")) {
                    // The match has been postponed.
                    List<Match> mlist = getMatches(getAndCondition(ml.get(i), "competition").append("status" , "TIMED"), new Document("_id", 0));
                    if (mlist.isEmpty()) {
                        continue;
                    }
                    final Integer index = nearestMatch(ml.get(i), mlist);
                    updateMatchDate(mlist.get(index).getMatchID(), ml.get(i).getMatchDate());
                    sDAO.updateBetsMatchPostponed(ml.get(i).getMatchID(), ml.get(i).getMatchDate());

                } else if (Objects.equals(ml.get(i).getStatus(), "FINISHED")) {
                    // The match is finished.
                    Integer id = replaceMatch(ml.get(i), false);
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
                            String status = m.getStatus();


                            if (Objects.equals(status, "TIMED")) {
                                // If it was timed I have to delete all non-confirmed slips in redis for consistency issues.
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
                    ml.get(i).setMatchID(mlist.get(0).getMatchID());
                    replaceMatch(ml.get(i), false);
                }
            }
            sDAO.closeConnection();
        }
    }

    /**
     * @param m  The target match.
     * @param ml The list of matches.
     * @return The index of the match in the list with the nearest matchDate of the target match.
     */

    private Integer nearestMatch(Match m, List<Match> ml) {
        final Instant dateMatch = stringToTimestamp(m.getMatchDate());
        Instant dateIter = stringToTimestamp(ml.get(0).getMatchDate());
        assert dateMatch != null;
        assert dateIter != null;

        long minDays = differenceSeconds(dateMatch , dateIter) , days;
        int minIndex = 0;
        for (int i = 1; i < ml.size(); i++) {
            if(!Objects.equals(ml.get(i).getStatus(), "TIMED")){
                continue;
            }

            days = differenceSeconds(
                    dateMatch ,
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
     * @param match The target match object.
     * @return True if a match with same date and same teams already is present in the database.
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
     * @param matchID The target match id.
     * @return The related match object.
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
