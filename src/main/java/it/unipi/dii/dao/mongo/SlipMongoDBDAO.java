package it.unipi.dii.dao.mongo;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.UpdateOptions;
import com.mongodb.client.model.Updates;
import it.unipi.dii.dao.SlipDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Slip;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.dateTimes.*;
import static it.unipi.dii.utility.converters.jsonToDocumentConverter.convertJsonToDocument;
import static it.unipi.dii.utility.converters.jsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.generators.randomGeneration.*;
import static it.unipi.dii.utility.mongoUtility.*;
import static it.unipi.dii.utility.converters.objectToJsonStringConverter.convertObjectToJsonString;

public class SlipMongoDBDAO extends BaseMongoDAO implements SlipDAO {
    /**
     * Adds a new slip in MongoDB.
     * The function fails if the slip is not valid.
     * @param slip The object slip to be inserted.
     * @return The new slipID value if it has been successfully inserted or -1 instead.
     */
    @Override
    public Integer addSlip(Slip slip) {
        if (slip.checkSlipValidity()) {
            slip.setSlipID(this.getLastID() + 1);
            List<Document> documents = new ArrayList<>();
            documents.add(Document.parse(convertObjectToJsonString(slip)));
            boolean x = insertDocuments(this.mongoDB.getCollection("slips"), documents);
            if (x) {
                return slip.getSlipID();
            } else {
                return -1;
            }
        }
        return -1;
    }

    /**
     * @return The biggest value of slipID from MongoDB.
     */
    public int getLastID() {
        // Firsts let's try with a limit on the confirmation date to speed up the query.
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("confirmationDate",
                                new Document("$gt", getCurrentDate().minusMonths(2).toString()))),
                new Document("$project",
                        new Document("slipID", 1L)
                                .append("_id", 0L)),
                new Document("$sort",
                        new Document("slipID", -1L)),
                new Document("$limit", 1L));
        AggregateIterable<Document> docs = this.mongoDB.getCollection("slips").aggregate(pipeline);
        try {
            return Objects.requireNonNull(docs.first()).getInteger("slipID");
        } catch (NullPointerException e1) {
            // Now we will try with no limit on the confirmation date.
            pipeline = Arrays.asList(new Document("$project",
                            new Document("slipID", 1L)
                                    .append("_id", 0L)),
                    new Document("$sort",
                            new Document("slipID", -1L)),
                    new Document("$limit", 1L));
            docs = this.mongoDB.getCollection("slips").aggregate(pipeline);

            try {
                return Objects.requireNonNull(docs.first()).getInteger("slipID");
            } catch (NullPointerException e2) {
                return 0;

            }
        }
    }

    /**
     * Remove all the slips from MongoDB that match the given criteria.
     *
     * @param query the match criteria.
     */
    @Override
    public void removeSlip(Document query) {
        deleteDocuments(
                this.mongoDB.getCollection("slips"),
                query
        );
    }

    /**
     * Remove the bet related to the input match from a slip in the database.
     *
     * @param slipID  The ID of the target slip.
     * @param matchID The ID of the target match.
     */
    @Override
    public void removeBet(String slipID, Integer matchID) {
        this.mongoDB.getCollection("slips").updateMany(
                new Document("SlipID", slipID).append("betsList.MatchID", matchID),
                new Document("$pull", new Document("betsList", new Document("MatchID", matchID)))
        );
    }

    /**
     * Remove all the bets in the database related to the target match.
     * This function make two write queries,
     * the first is the update one, the second is used to delete all the slips with no bets.
     *
     * @param matchID The ID of the target match.
     */
    @Override
    public void removeAllBetsOfMatch(Integer matchID) {
        if (matchID >= 0) {
            MongoCollection<Document> slips_coll = this.mongoDB.getCollection("slips");
            // Update the slips with a bet on the target match.
            slips_coll.updateMany(
                    new Document(),
                    Updates.pull("betsList", Filters.eq("matchID", matchID))
            );
            // Delete all the slips with no bets.
            slips_coll.deleteMany(Filters.size("betsList", 0));
        }
    }

    /**
     * Check if a bet has a successful result or not.
     *
     * @param slip    The slip object to be checked.
     * @param matchID The match ID related to the bet that we want to check.
     * @param mb      MatchMongoDBDAO Object.
     * @param cb      CustomerMongoDBDAO Object.
     */

    private void checkBetWin(Slip slip, Integer matchID, MatchMongoDBDAO mb, CustomerMongoDBDAO cb) {
        int betListSize = slip.betsList.size();
        for (int i = 0; i < betListSize; i++) {
            if (Objects.equals(matchID, slip.betsList.get(i).getMatchID())) {
                slip.betsList.get(i).setWin(
                        mb.getMatch(matchID)
                                .checkMultiplierWin(
                                        slip.betsList.get(i).getChosenMultiplierName()
                                )
                );
                int winSlip = checkIfSlipWin(slip);
                if (winSlip == 0 || winSlip == 1 || winSlip == 2) {
                    if (winSlip == 2) {
                        winSlip = 0;
                    }
                    slip.setWin(winSlip);
                    slip.setAmount(truncateNumber(slip.getAmount(), 3));
                } else {
                    slip.setWin(winSlip);
                }
                substituteSlip(slip.getSlipID(), slip);
                if (winSlip == 1) {
                    cb.redeem(slip.getUsername(), slip.getAmount());
                }
                return;
            }
        }
    }

    /**
     * Substitute the slip with slipID = slipID with the input slip.
     *
     * @param slipID The ID of the target slip.
     * @param slip   The new slip object.
     */

    private void substituteSlip(Integer slipID, Slip slip) {
        this.mongoDB.getCollection("slips").replaceOne(
                new Document("slipID", slipID),
                convertJsonToDocument(convertObjectToJsonString(slip))
        );
    }

    /**
     * This function is called when the target match is finished.
     * This function loop all the slips in the database, and check if they have a bet related to the target match.
     * If so, the bet will be evaluated.
     *
     * @param matchID The ID of the target match.
     */

    public void checkSlipsWhenMatchEnds(Integer matchID) {
        if (matchID >= 0) {
            List<Slip> sl = getSlips(
                    new Document("betsList", new Document("$elemMatch", new Document("matchID", matchID))),
                    new Document("_id", 0)
            );
            CustomerMongoDBDAO cs = new CustomerMongoDBDAO();
            MatchMongoDBDAO mb = new MatchMongoDBDAO();
            cs.openConnection();
            mb.openConnection();
            for (Slip slip : sl) {
                checkBetWin(slip, matchID, mb, cs);
            }
            cs.closeConnection();
            mb.closeConnection();
        }
    }

    /**
     * When a match is postponed, every matchDate in the bets (for that match) must be updated.
     * @param matchID The ID of the postponed match.
     */
    public void updateBetsMatchPostponed(Integer matchID, String newDate) {
        Document filter = new Document("betsList.matchID", matchID);
        Document update = new Document("$set", new Document("betsList.$[elem].matchDate", newDate));
        UpdateOptions options = new UpdateOptions().arrayFilters(
                List.of(
                        new Document("elem.matchID", matchID)
                )
        );
        this.mongoDB.getCollection("slips").updateMany(filter, update, options);
    }

    /**
     * Query the database and return the wanted slips.
     *
     * @param query      Match criteria.
     * @param projection Projection criteria.
     * @return The slips list that match the criteria.
     */

    @Override
    public List<Slip> getSlips(Document query, Document projection) {
        List<Slip> s_list = new ArrayList<>();
        try (MongoCursor<Document> cursor = this.mongoDB.getCollection("slips").find(query).projection(projection).iterator()) {
            while (cursor.hasNext()) {
                Document document = cursor.next();
                Slip s = convertJsonToObject(document.toJson(), Slip.class);
                // Take the competition_id, the home teams, the away team, the match date and the result (win)
                // from each bet in the Slip contained in document and set them in Slip s
                for (int i = 0; i < Objects.requireNonNull(s).betsList.size(); i++) {
                    s.betsList.get(i).setCompetition_id(document.getList("betsList", Document.class).get(i).getString("competition_id"));
                    s.betsList.get(i).setTeamHome(document.getList("betsList", Document.class).get(i).getString("teamHome"));
                    s.betsList.get(i).setTeamAway(document.getList("betsList", Document.class).get(i).getString("teamAway"));
                    s.betsList.get(i).setMatchDate(document.getList("betsList", Document.class).get(i).getString("matchDate"));
                    s.betsList.get(i).setWin(document.getList("betsList", Document.class).get(i).getInteger("win"));
                }
                s_list.add(s);
            }
        } catch (NullPointerException e) {
            return null;
        }
        return s_list;
    }

    /**
     * Query the database and return the wanted slip.
     *
     * @param slipID The ID of the target slip.
     * @return The slip object related to the ID, or null if the ID does not belong to any slip.
     */

    public Slip getSlip(Integer slipID) {
        List<Slip> ml = getSlips(
                new Document("slipID", slipID),
                new Document("_id", 0)
        );

        if (ml.isEmpty()) {
            return null;
        } else {
            return ml.get(0);
        }
    }

    /**
     * Check if a slip has a successful result or not.
     * @param slip The slip object to check.
     * @return The result of the slip or -1 if it is impossible to evaluate it.
     */

    public int checkIfSlipWin(Slip slip) {
        int betListSize = slip.betsList.size();
        boolean winSlip = true;
        double betAmount = slip.getBetAmount(); // The bet amount of the slip.
        int p = 0;
        for (int i = 0; i < betListSize; i++) {
            if (slip.betsList.get(i).getWin() == 1) {
                // The bet has a positive result.
                betAmount = betAmount * slip.betsList.get(i).getChosenMultiplierValue();


            } else if (slip.betsList.get(i).getWin() == 0) {
                // The bet has a negative result.
                betAmount = 0;
                winSlip = false; // the slip is lost.

            } else {
                slip.setAmount(0);
                // Some of the bets in this slip has not ended yet
                p = -1;
            }
        }
        slip.setAmount(truncateNumber(betAmount, 2));
        if (p == -1) {// there is a match that is not ended yet
            if (!winSlip) { // there is a match that is not ended yet and the slip is lost
                return 2;
            }
            // there is a match that is not ended yet and the slip is not lost yet
            return -1;
        }
        if (winSlip) {
            return 1;
        } else {
            return 0;
        }
    }

    //Redis
    public void deleteBetFromSlip(String username, Integer slipID, Bet bet) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public String slipBetsKeysNS(String username, Integer slipID) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public String slipbetAmountKeysNS(String username, Integer slipID) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public String slipcreationDateKeysNS(String username, Integer slipID) {

        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public String UsernameKeysNS(String username) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public String slipKeysNS(String username, Integer slipID) {

        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void persist(Slip slip) { //renew the expiration time of a specific slip

        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public Slip load(String username, Integer slipID, double betAmount) { //gets bets from a specific slip


        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public int create_Slip(Slip slip) { //data una slip, crea una chiave e la inserisce nel database

        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void refreshTTL(String userID, String slipID) {

        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void delete_Slip(String username, Integer slipID) {  //delete slip from redis

        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public int addBetToSlip(String username, Integer slipID, Bet bet) {

        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public boolean sendConfirmedSlipToMongo(String username, Integer slipID, double betAmount) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public List<String> getAllUsernames() {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    @Override
    public void replaceSlip(Integer slipID, Slip slip) {
        throw new UnsupportedOperationException("Not supported in MongoDB implementation");
    }
}

