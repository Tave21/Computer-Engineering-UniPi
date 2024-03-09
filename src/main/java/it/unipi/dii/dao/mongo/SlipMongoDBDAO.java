package it.unipi.dii.dao.mongo;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import it.unipi.dii.dao.SlipDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Slip;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.DateTimes.*;
import static it.unipi.dii.utility.JsonToDocument.convertJsonToDocument;
import static it.unipi.dii.utility.JsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.MongoUtility.*;
import static it.unipi.dii.utility.ObjectToJsonString.convertObjectToJsonString;
import static it.unipi.dii.utility.RandomNumber.truncateNumber;

public class SlipMongoDBDAO extends BaseMongoDAO implements SlipDAO {
    /**
     * Adds a new slip in MongoDB.
     * This function fails if the slip is not valid.
     *
     * @param slip The object slip to be inserted.
     */
    @Override
    public Integer addSlip(Slip slip) {
        if (slip.checkSlipValidity()) {
            slip.setSlipID(this.getLastID() + 1);
            List<Document> documents = new ArrayList<>();
            documents.add(Document.parse(convertObjectToJsonString(slip)));
            boolean x = insertDocuments(this.mongoDB.getCollection("slips"), documents);
            if(x) {
                return slip.getSlipID();
            } else {
                return -1;
            }


        }
        return -1;
    }



    private int getLastID() {
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("confirmationDate",
                                new Document("$gt", getCurrentDate().minusMonths(4).toString()))),
                new Document("$project",
                        new Document("slipID", 1L)
                                .append("_id", 0L)),
                new Document("$sort",
                        new Document("slipID", -1L)),
                new Document("$limit", 1L));
        AggregateIterable<Document> docs = this.mongoDB.getCollection("slips").aggregate(pipeline);
        try {
            return Objects.requireNonNull(docs.first()).getInteger("slipID");
        }catch(NullPointerException e){
            return 0;
        }
    }

    /**
     * Remove all the slips from MongoDB that match the criteria.
     *
     * @param query the match criteria.
     */
    @Override
    public void removeSlip(Document query) {
        MongoCollection<Document> slips_coll = this.mongoDB.getCollection("slips");
        deleteDocuments(slips_coll, query);
    }

    @Override
    public void replaceSlip(Integer slipID, Slip slip) {
        throw new UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    /**
     * Remove the bet related to the input match from a slip in the database.
     *
     * @param slipID  The id of the target slip.
     * @param matchID The target match.
     */
    @Override
    public void removeBet(String slipID, Integer matchID) {
        MongoCollection<Document> slips_coll = this.mongoDB.getCollection("slips");
        Document updateQuery = new Document("SlipID", slipID).append("betsList.MatchID", matchID);
        Document update = new Document("$pull", new Document("betsList", new Document("MatchID", matchID)));
        slips_coll.updateMany(updateQuery, update);
    }

    /**
     * Remove all the bets in the database related to the target match.
     *
     * @param matchID ID of the target match.
     */
    @Override
    public void removeAllBetsOfMatch(Integer matchID) {
        MongoCollection<Document> slips_coll = this.mongoDB.getCollection("slips");
        Document updateQuery = new Document("betsList.MatchID", matchID);
        Document update = new Document("$pull", new Document("betsList", new Document("matchID", matchID)));
        slips_coll.updateMany(updateQuery, update);
    }

    /**
     * Check if the slips in the list have a successful result or not.
     *
     * @param slips     List of slip to check.
     * @param recompute If true, all the bets result are recomputed, put false for a less precise but faster computation.
     */
    public void checkSlipWin(List<Slip> slips, boolean recompute) {
        CustomerMongoDBDAO cs = new CustomerMongoDBDAO();
        MatchMongoDBDAO mb = new MatchMongoDBDAO();

        cs.openConnection();
        mb.openConnection();
        for (Slip slip : slips) {
            int betListSize = slip.betsList.size();
            int winSlip = 1;
            boolean validate = true;
            double amount = slip.getBetAmount();

            for (int i = 0; i < betListSize; i++) {
                if (slip.betsList.get(i).getWin() == 1 && !recompute) {
                    amount = amount * slip.betsList.get(i).getChosenMultiplierValue(); // The bet has a positive result.
                } else if (slip.betsList.get(i).getWin() == 0 && !recompute) {
                    amount = 0; // The bet has a negative result.
                    winSlip = 0; // the slip is lost.
                } else {
                    // the result of the slip must be determined.
                    int b = checkBetWin(slip, slip.betsList.get(i).getMatchID(), false, mb, cs);
                    if (b == 1) {
                        amount = amount * slip.betsList.get(i).getChosenMultiplierValue();  // The bet has a positive result.
                    } else if (b == 0) {
                        amount = 0; // The bet has a negative result, so the winning amount goes to 0.
                        winSlip = 0; // So the slip is lost.
                    } else {
                        b = -1; // The bet has not a result yet.
                        validate = false;  // The slip is not valid yet to be evaluated.
                    }
                    slip.betsList.get(i).setWin(b);
                }
            }
            if (validate) {
                // The slip has been evaluated.
                slip.setWin(winSlip);
                slip.setAmount(truncateNumber(amount, 2));
                substituteSlip(slip.getSlipID(), slip); // Update MongoDB.
                if (winSlip == 1) {
                    // If the slip has a positive result.
                    cs.redeem(slip.getUsername(), slip.getAmount()); // The customer receive the money.
                }
            }
        }
        mb.closeConnection();
        cs.closeConnection();
    }

    /**
     * Check if a bet has a successful result or not.
     *
     * @param slip    The slip to be checked.
     * @param matchID The match related to the bet.
     * @param update  If true, the update is written to the database too.
     * @param mb      MatchMongoDBDAO Object.
     * @param cb      CustomerMongoDBDAO Object.
     * @return An integer that represent the result of the bet: <br>
     * 1 --> The bet has been won. <br>
     * 0 --> The bet has been lost. <br>
     * -1 --> Impossible to evaluate, probably the match related to the bet is not finished yet.
     */

    private int checkBetWin(Slip slip, Integer matchID, boolean update, MatchMongoDBDAO mb, CustomerMongoDBDAO cb) {
        int betListSize = slip.betsList.size();
        for (int i = 0; i < betListSize; i++) {
            if (Objects.equals(matchID, slip.betsList.get(i).getMatchID())) {
                slip.betsList.get(i).setWin(
                        mb.getMatch(matchID)
                                .checkMultiplierWin(
                                        slip.betsList.get(i).getChosenMultiplierName()
                                )
                );
                if (update) {
                    //System.out.println("Prima della checkif "+slip.getAmount());
                    int winSlip = checkIfSlipWin(slip);
                    //System.out.println("DOPo la checkif "+slip.getAmount());

                    if (winSlip == 0 || winSlip == 1 || winSlip == 2){
                        if(winSlip == 2){
                            winSlip = 0;
                        }
                        slip.setWin(winSlip);
                        slip.setAmount(truncateNumber(slip.getAmount(), 3));
                    } else {
                        slip.setWin(winSlip);
                    }
                    //System.out.println(slip);
                    substituteSlip(slip.getSlipID(), slip);
                    if (winSlip == 1) {
                        cb.redeem(slip.getUsername(), slip.getAmount());
                    }
                }
                return slip.betsList.get(i).getWin();
            }
        }
        return -1;
    }

    /**
     * Substitute the slip with slipID = slipID with the input slip.
     *
     * @param slipID The id of slip to be substituted.
     * @param slip   The new slip.
     */

    private void substituteSlip(Integer slipID, Slip slip) {
        MongoCollection<Document> slips_coll = this.mongoDB.getCollection("slips");
        Document query = new Document("slipID", new Document("$eq", slipID));
        slips_coll.replaceOne(query, convertJsonToDocument(convertObjectToJsonString(slip)));
    }

    /**
     * This function is called when the target match is finished.
     * This function loop all the slips in the database, and check if they have a bet related to the target match.
     * If so, the bet will be evaluated.
     *
     * @param matchID The target match.
     */

    public void checkSlipsWhenMatchEnds(Integer matchID) {
        Document query = new Document("betsList", new Document("$elemMatch", new Document("matchID", matchID)));
        List<Slip> sl = getSlips(query, new Document("_id", 0));
        CustomerMongoDBDAO cs = new CustomerMongoDBDAO();
        MatchMongoDBDAO mb = new MatchMongoDBDAO();
        cs.openConnection();
        mb.openConnection();
        for (Slip slip : sl) {
            checkBetWin(slip, matchID, true, mb, cs);
        }
        cs.closeConnection();
        mb.closeConnection();
    }

    /**
     * When a match is postponed, every matchDate in the bets (for that match) must be updated.
     *
     * @param matchID The postponed match ID.
     */
    public void updateBetsMatchPostponed(Integer matchID, String newDate) {

        List<Document> pipeline = Arrays.asList(new Document("$set",
                        new Document("betsList",
                                new Document("$map",
                                        new Document("input", "$betsList")
                                                .append("as", "bet")
                                                .append("in",
                                                        new Document("$cond",
                                                                new Document("if",
                                                                        new Document("$eq", Arrays.asList("$$bet.matchID", matchID)))
                                                                        .append("then",
                                                                                new Document("$mergeObjects", Arrays.asList("$$bet",
                                                                                        new Document("matchDate", newDate))))
                                                                        .append("else", "$$bet")))))),
                new Document("$out", "slips"));

        this.mongoDB.getCollection("slips").aggregate(pipeline).toCollection();
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
        MongoCollection<Document> slips_coll = this.mongoDB.getCollection("slips");
        List<Slip> s_list = new ArrayList<>();
        try (MongoCursor<Document> cursor = slips_coll.find(query).projection(projection).iterator()) {
            while (cursor.hasNext()) {
                Document document = cursor.next();
                Slip s = convertJsonToObject(document.toJson(), Slip.class);
                //take competition_id,teamHome,teamAway, matchDate,win from each bet in the Slip contained in document and set them in Slip s
                for (int i = 0; i < s.betsList.size(); i++) {
                    s.betsList.get(i).setCompetition_id(document.getList("betsList", Document.class).get(i).getString("competition_id"));
                    s.betsList.get(i).setTeamHome(document.getList("betsList", Document.class).get(i).getString("teamHome"));
                    s.betsList.get(i).setTeamAway(document.getList("betsList", Document.class).get(i).getString("teamAway"));
                    s.betsList.get(i).setMatchDate(document.getList("betsList", Document.class).get(i).getString("matchDate"));
                    s.betsList.get(i).setWin(document.getList("betsList", Document.class).get(i).getInteger("win"));
                }
                s_list.add(s);
            }
        }
        return s_list;
    }

    /**
     * Query the database and return the wanted slip.
     * @param slipID The id of the slip to get.
     * @return The object slip.
     */

    public Slip getSlip(Integer slipID) {
        List<Slip> ml = getSlips(new Document("slipID", new Document("$eq", slipID)), new Document("_id", 0));
        if (ml.isEmpty()) {
            return null;
        } else {
            return ml.get(0);
        }
    }

    /**
     * Check if a slip has a successful result or not.
     *
     * @param slip The slip to check.
     * @return The result of the slip or -1 if it is impossible to evaluate it.
     */

    public int checkIfSlipWin(Slip slip) {

        int betListSize = slip.betsList.size();
        boolean winSlip = true;

        double amount = slip.getBetAmount();

        int p = 0;
        for (int i = 0; i < betListSize; i++) {
            if (slip.betsList.get(i).getWin() == 1) {
                // The bet has a positive result.
                amount = amount * slip.betsList.get(i).getChosenMultiplierValue();


            } else if (slip.betsList.get(i).getWin() == 0) {
                // The bet has a negative result.
                amount = 0;
                winSlip = false; // the slip is lost.

            } else {
                slip.setAmount(0);
                // Some of the bets in this slip has not ended yet
                p = -1;
            }
        }
        slip.setAmount(truncateNumber(amount, 2));
        if(p == -1){// there is a match that is not ended yet
            if(!winSlip){ // there is a match that is not ended yet and the slip is lost
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

    public Slip load(String username, Integer slipID , double betAmount) { //gets bets from a specific slip


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
}

