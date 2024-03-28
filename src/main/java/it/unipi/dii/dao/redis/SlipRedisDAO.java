package it.unipi.dii.dao.redis;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.mongodb.client.MongoCollection;
import it.unipi.dii.dao.SlipDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import it.unipi.dii.dao.base.BaseRedisDAO;
import it.unipi.dii.dao.mongo.SlipMongoDBDAO;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Slip;
import org.bson.Document;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.params.ScanParams;
import redis.clients.jedis.resps.ScanResult;

import java.io.IOException;
import java.util.*;

import static it.unipi.dii.utility.dateTimes.getCurrentInstantString;


public class SlipRedisDAO extends BaseRedisDAO implements SlipDAO {
    public static final String APP_NS = "BeansBet";
    public static final int EXPIRATION_IN_SEC = 10800; //temporary slips are alive only for one day
    private static final String NS = "cust"; //namespace

    //EXAMPLE OF KEY FOR A SPECIFIC SLIP OF A SPECIFIC USER:
    //  BeansBetcust:niccolo:slip:3:betAmount = 100
    //  BeansBetcust:niccolo:slip:3:creation_date= "2021-10-10"
    //  BeansBetcust:niccolo:slip:3:options=
    //"[{\"competition_id\":\"null\",\"matchID\":0,\"teamHome\":\"West Ham United FC\",\"teamAway\":\"Brentford FC\",
    // \"chosenMultiplierName\":\"Over1/5\"\"chosenMultipliervalue\":2.2,\"matchDate\":\"2024-02-26T20:00:00Z\",\"win\":-1}]"

    @Override
    public String slipBetsKeysNS(String username, Integer slipID) {
        return APP_NS + NS + ":" + username + ":" + slipID + ":bets";
    }

    @Override
    public String slipbetAmountKeysNS(String username, Integer slipID) {
        return APP_NS + NS + ":" + username + ":" + slipID + ":betAmount";
    }

    @Override
    public String slipcreationDateKeysNS(String username, Integer slipID) {
        return APP_NS + NS + ":" + username + ":" + slipID + ":creation_date";
    }

    @Override
    public String UsernameKeysNS(String username) {
        return APP_NS + NS + ":" + username + ":";
    }

    public String slipKeysNS(String username, Integer slipID) {
        return APP_NS + NS + ":" + username + ":" + slipID + ":";
    }


    @Override
    public void persist(Slip slip) { //renew the expiration time of a specific slip
        try (Jedis jedis = getConnection()) {
            create_Slip(slip);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Slip load(String username, Integer slipID , double betAmount) { // get a specific slip
        String SlipKey = slipBetsKeysNS(username, slipID);
        String key = slipcreationDateKeysNS(username, slipID);
        try (Jedis jedis = getConnection()) {
            String data = jedis.get(SlipKey); // Get the bet by using the key.
            if (data != null) {
                Slip slip = new Slip();
                slip.setSlipID(slipID);
                slip.setCreationDate(jedis.get(key));
                slip.setBetAmount(betAmount);
                slip.setConfirmationDate(getCurrentInstantString());
                slip.setUsername(username);
                slip.setBetsList(fromJsonToBetList(data));

                return slip;
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return null;
    }

    public List<String> getAllUsernames() {
        Jedis jedis = getConnection();
        Set<String> keys = jedis.keys(APP_NS + NS + ":*");
        List<String> usernames = new ArrayList<>();
        for (String key : keys) {
            String[] keyParts = key.split(":");
            String username = keyParts[1];
            if (!usernames.contains(username)) {usernames.add(username);}
        }
        return usernames;
    }
    //it returns a list of slips from a username
    public List<Slip> getListFromUser(String username) throws IOException {
        Jedis jedis = getConnection();
        String keyPattern = UsernameKeysNS(username) + "*";
        List<Slip> slipList = new ArrayList<>();
        ScanParams scanParams = new ScanParams().match(keyPattern);

        String cursor = ScanParams.SCAN_POINTER_START;

        Set<String> uniqueIds = new HashSet<>();
        do {
            ScanResult<String> scanResult = jedis.scan(cursor, scanParams);

            for (String key : scanResult.getResult()) {
                String[] keyParts = key.split(":");
                if (keyParts.length >= 4) {
                    String slipId = keyParts[2];
                    uniqueIds.add(slipId);
                }
            }
            cursor = scanResult.getCursor();
        } while (!cursor.equals("0"));

        for (String slipId : uniqueIds) {
            String bets = jedis.get(slipBetsKeysNS(username, Integer.parseInt(slipId)));
            String betAmount = jedis.get(slipbetAmountKeysNS(username, Integer.parseInt(slipId)));
            String creationDate = jedis.get(slipcreationDateKeysNS(username, Integer.parseInt(slipId)));
            Slip slip = new Slip(username, null, creationDate, Double.parseDouble(betAmount));
            slip.setSlipID(Integer.parseInt(slipId));
            slip.setBetsList(fromJsonToBetList(bets));
            slipList.add(slip);
        }
        return  slipList;
    }

    //creation of a slip in Redis
    @Override
    public int create_Slip(Slip slip) {

        Bet bet = slip.findBetsList().get(0);
        BaseMongoDAO baseMongoDAO = new BaseMongoDAO();
        baseMongoDAO.openConnection();
        MongoCollection<Document> matchesCollection = baseMongoDAO.mongoDB.getCollection("matches");
        Document query = new Document("team_away", bet.getTeamAway())
                .append("team_home", bet.getTeamHome());
        Document sort = new Document("matchDate", -1); //descending order
        Document result = matchesCollection.find(query).sort(sort).first();
        if(Objects.equals(result.getString("status"), "IN_PLAY")){
            return 1;
        }
        //keys creation
        String username = slip.getUsername();
        String SlipKeyBets = slipBetsKeysNS(username, slip.getSlipID());
        String SlipKeybetamount = slipbetAmountKeysNS(username, slip.getSlipID());
        String SlipKeycreationdate = slipcreationDateKeysNS(username, slip.getSlipID());
        double betAmount = slip.getBetAmount();

        try (Jedis jedis = getConnection()) {
            jedis.set(slipbetAmountKeysNS(username, slip.getSlipID()), String.valueOf(betAmount));
            jedis.set(SlipKeycreationdate, slip.getCreationDate());
            jedis.set(SlipKeyBets, writeToJsonFileBets(slip.findBetsList()));
            jedis.expire(SlipKeybetamount, EXPIRATION_IN_SEC);
            jedis.expire(SlipKeycreationdate, EXPIRATION_IN_SEC);
            jedis.expire(SlipKeyBets, EXPIRATION_IN_SEC);
        }
        return 0;
    }


    @Override
    public void refreshTTL(String userID, String slipID) {
        //it retrieves all the IDs associated with the username
        Jedis jedis = getConnection();
        Set<String> ids = jedis.keys("BeansBetcust:" + userID + slipID+  ":*");

        // Set the new TTL for each ID
        for (String id : ids) {
            jedis.expire(id , EXPIRATION_IN_SEC);
            jedis.expire(id , EXPIRATION_IN_SEC);
            jedis.expire(id , EXPIRATION_IN_SEC);
        }
    }

    @Override
    public void delete_Slip(String username, Integer slipID) {  //delete slip from redis
        String slipkey = "BeansBetcust:"+username+":"+ slipID;
        String SlipKeyBets= slipBetsKeysNS(username, slipID);
        String SlipKeybetamount = slipbetAmountKeysNS(username, slipID);
        String SlipKeycreationdate = slipcreationDateKeysNS(username, slipID);
        Jedis jedis = getConnection();
        jedis.del(SlipKeybetamount);
        jedis.del(SlipKeyBets);
        jedis.del(SlipKeycreationdate);
        jedis.del(slipkey);

    }

    @Override
    public int addBetToSlip(String username, Integer slipID, Bet bet) {

        BaseMongoDAO baseMongoDAO = new BaseMongoDAO();
        baseMongoDAO.openConnection();
        MongoCollection<Document> matchesCollection = baseMongoDAO.mongoDB.getCollection("matches");
        Document query = new Document("team_away", bet.getTeamAway())
                .append("team_home", bet.getTeamHome());
        Document sort = new Document("matchDate", -1); // descending order
        Document result = matchesCollection.find(query).sort(sort).first();
        if(Objects.equals(result.getString("status"), "IN_PLAY")){
            return 1;
        }

        try(Jedis jedis = getConnection()) {
            String jsonStr = jedis.get(slipBetsKeysNS(username, slipID));
            List<Bet> betList = fromJsonToBetList(jsonStr);
            betList.add(bet);
            jedis.set(slipBetsKeysNS(username, slipID), writeToJsonFileBets(betList));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return 0;

    }

    @Override
    public void deleteBetFromSlip(String username, Integer slipID, Bet bet) {
        try (Jedis jedis = getConnection()) {
            String jsonStr = jedis.get(slipBetsKeysNS(username, slipID));
            List<Bet> betList = fromJsonToBetList(jsonStr);
            if (!betList.isEmpty()) {
                betList.remove(bet);
                jedis.set(slipBetsKeysNS(username, slipID), writeToJsonFileBets(betList));
            } else {
                jedis.del(slipBetsKeysNS(username, slipID));
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


    @Override
    public boolean sendConfirmedSlipToMongo(String username, Integer slipID , double betAmount) {
        SlipMongoDBDAO slipMongoDBDAO = new SlipMongoDBDAO();
        slipMongoDBDAO.openConnection();
        Slip slip = load(username, slipID , betAmount);

        slip.setBetsWinToMinus1(); // Initialize the slip.

        //for that takes single bet from slip and modifies each matchID of that bet
        for (Bet bet : slip.findBetsList()) {

            BaseMongoDAO baseMongoDAO = new BaseMongoDAO();
            baseMongoDAO.openConnection();
            MongoCollection<Document> matchesCollection = baseMongoDAO.mongoDB.getCollection("matches");

            Document query = new Document("team_away", bet.getTeamAway())
                    .append("team_home", bet.getTeamHome());
            Document sort = new Document("matchDate", -1); // descending order
            Document result = matchesCollection.find(query).sort(sort).first();
            bet.setMatchID(result.getInteger("matchID"));
            bet.setCompetition_id(result.getString("competition_id"));
        }

        int x = slipMongoDBDAO.addSlip(slip); // Add the slip to MongoDB.

        if(x == -1){
            //slip not inserted, user will see an error message for failed insertion
            slipMongoDBDAO.closeConnection();
            return false; // no need to delete the slip from Redis
        }
        slipMongoDBDAO.closeConnection();
        delete_Slip(username, slipID); // Delete the slip from Redis.
        return true;
    }


    public static String writeToJsonFileBets(List<Bet> betList) {

        try {
            ObjectMapper objectMapper = new ObjectMapper();
            return objectMapper.writeValueAsString(betList);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static List<Bet> fromJsonToBetList(String jsonString) throws IOException {
        //creation of an ObjectMapper object of Jackson
        ObjectMapper objectMapper = new ObjectMapper();

        //conversion of the JSON string into a list of Bet objects
        return objectMapper.readValue(jsonString, new TypeReference<List<Bet>>() {
        });
    }


    //Mongo
    public Integer addSlip(Slip slip) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void removeSlip(Document query) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void addBet(Slip slip, Bet bet) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void updateBet(String userId, Bet bet) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void removeBet(String slipId, Integer matchID) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void removeAllBetsOfMatch(Integer matchID) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void replaceSlip(Integer slipID, Slip slip) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    // void checkSlipsWhenMatchEnds(Integer matchID);
    public List<Slip> getSlips(Document query, Document projection) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public Slip getSlip(Integer slipID) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }
}


