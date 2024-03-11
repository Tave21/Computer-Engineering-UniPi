package it.unipi.dii.dao.redis;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dii.dao.PollDAO;
import it.unipi.dii.dao.base.BaseRedisDAO;
import it.unipi.dii.dao.mongo.PollMongoDBDAO;
import it.unipi.dii.model.Poll;
import it.unipi.dii.model.pollOption;
import org.bson.Document;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.params.ScanParams;
import redis.clients.jedis.resps.ScanResult;

import java.io.IOException;
import java.time.Duration;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class PollRedisDAO extends BaseRedisDAO implements PollDAO {


    // key --> BeansBet:poll:3:options= [option1, option2, option3...]
    //         BeansBet:poll:3:creationDate= "2022-01-01"
    //         BeansBet:poll:3:pollType= "Best Player"
    //         BeansBet:poll:3:pollName= "Miglior giocatore dell'anno?"
    //         BeansBet:poll:3:activationDate= "2022-01-01"
    //         BeansBet:poll:3:optionNumber= 3
    //         BeansBet:poll:3:numberOfVotes= 100
    //         BeansBet:pollcookie:username= "..."
    //esempio --> BeansBet:3:options= ["Ronaldo","Messi","Halland"]

    public static final String APP_NS = "BeansBet";
    private static final String NS = "admin";
    public static final int EXPIRATION_IN_SEC = 86400;
    public static final int EXPIRATION_IN_SEC_COOKIE = 129600;

    public String pollIDKeysNS() {
        return APP_NS + NS + ":" ;
    }
    public String pollIDRealKeysNS(Integer pollID) {
        return APP_NS + NS + ":" + pollID + ":";
    }

    public String pollNameKeysNS(Integer pollID) {
        return APP_NS + NS + ":" + pollID + ":pollName";
    }

    public String creationDateKeysNS(Integer pollID) {
        return APP_NS + NS + ":" + pollID + ":creationDate";
    }

    public String optionNumberKeysNS(Integer pollID) {
        return APP_NS + NS + ":" + pollID + ":optionNumber";
    }

    public String numberOfVotesKeysNS(Integer pollID) {
        return APP_NS + NS + ":" + pollID + ":numberOfVotes";
    }

    public String activationDateKeysNS(Integer pollID) {
        return APP_NS + NS + ":" + pollID + ":activationDate";
    }

    public String pollTypeKeysNS(Integer pollID) {
        return APP_NS + NS + ":" + pollID + ":pollType";
    }

    public String optionsKeysNS(Integer pollID) {
        return APP_NS + NS + ":" + pollID + ":options";
    }

    public void addPollToRedis(Poll poll) {
        try( Jedis jedis = getConnection()) {
            jedis.set(pollNameKeysNS(poll.getPollID()), poll.getPollName());
            jedis.set(creationDateKeysNS(poll.getPollID()), poll.getCreationDate());
            jedis.set(pollTypeKeysNS(poll.getPollID()), poll.getPollType());
            jedis.set(activationDateKeysNS(poll.getPollID()), poll.getActivationDate());
            //managing of TTL in order to start counting one day life from activation date
            ZonedDateTime activationdate= ZonedDateTime.parse(poll.getActivationDate());
            ZonedDateTime creationDate = ZonedDateTime.parse(poll.getCreationDate());
            Duration duration = Duration.between(creationDate, activationdate);
            long diff = duration.getSeconds();
            jedis.set(numberOfVotesKeysNS(poll.getPollID()), poll.getNumberOfVotes().toString());
            jedis.set(optionsKeysNS(poll.getPollID()), writeToJsonFileOptions(poll.getOptions()));
            jedis.expire(pollNameKeysNS(poll.getPollID()), EXPIRATION_IN_SEC+diff);
            jedis.expire(creationDateKeysNS(poll.getPollID()), EXPIRATION_IN_SEC+diff);
            jedis.expire(pollTypeKeysNS(poll.getPollID()), EXPIRATION_IN_SEC+diff);
            jedis.expire(activationDateKeysNS(poll.getPollID()), EXPIRATION_IN_SEC +diff);
            jedis.expire(numberOfVotesKeysNS(poll.getPollID()), EXPIRATION_IN_SEC+diff);
            jedis.expire(optionsKeysNS(poll.getPollID()), EXPIRATION_IN_SEC+diff);
        } catch (Exception e) {
            e.printStackTrace();
        }


    }


    public List<Poll> getAllPollFromRedis() {
        try (Jedis jedis = getConnection()) {
            String keyPattern = pollIDKeysNS() + "*";
            List<Poll> pollList = new ArrayList<>();
            ScanParams scanParams = new ScanParams().match(keyPattern);

            String cursor = ScanParams.SCAN_POINTER_START;

            Set<String> uniqueIds = new HashSet<>();
            do {
                ScanResult<String> scanResult = jedis.scan(cursor, scanParams);

                for (String key : scanResult.getResult()) {
                    String[] keyParts = key.split(":");
                    String pollID = keyParts[1];
                    uniqueIds.add(pollID);
                }
                cursor = scanResult.getCursor();
            } while (!cursor.equals("0"));

            for (String pollID : uniqueIds) {
                String options = jedis.get(optionsKeysNS(Integer.parseInt(pollID)));
                String pollName = jedis.get(pollNameKeysNS(Integer.parseInt(pollID)));
                String creationDate = jedis.get(creationDateKeysNS(Integer.parseInt(pollID)));
                String activationDate = jedis.get(activationDateKeysNS(Integer.parseInt(pollID)));
                String pollType = jedis.get(pollTypeKeysNS(Integer.parseInt(pollID)));
                Poll poll = new Poll(pollName, pollType, creationDate, activationDate);
                poll.setPollID(Integer.parseInt(pollID));
                poll.setOptions(fromJsonToOptionList(options));
                pollList.add(poll);
            }

            return pollList;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public String getPollCookieOfUser(String username) {
        try (Jedis jedis = getConnection()) {
            return jedis.get(APP_NS  + ":pollcookie:" + username);
        }
    }
    public void createPollCookieOfUser(String username, String cookie) {
        try (Jedis jedis = getConnection()) {
            jedis.set(APP_NS  + ":pollcookie:" + username, cookie);
            jedis.expire(APP_NS  + ":pollcookie:" + username, EXPIRATION_IN_SEC_COOKIE);
        }
    }

    public void refreshTTL(String username) {
        //it retrieves all the IDs associated with the username
        Jedis jedis = getConnection();
        String ids = APP_NS + ":pollcookie:" + username;
        jedis.expire(ids, EXPIRATION_IN_SEC_COOKIE);
    }

    public static String writeToJsonFileOptions(List<pollOption> optionList) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            return objectMapper.writeValueAsString(optionList);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void removePollfromRedis(Integer pollID) {
        try(Jedis jedis = getConnection()) {
            jedis.del(pollNameKeysNS(pollID));
            jedis.del(creationDateKeysNS(pollID));
            jedis.del(pollTypeKeysNS(pollID));
            jedis.del(activationDateKeysNS(pollID));
            jedis.del(optionNumberKeysNS(pollID));
            jedis.del(numberOfVotesKeysNS(pollID));
            jedis.del(optionsKeysNS(pollID));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void addOption(Integer pollID, pollOption option) {

        try(Jedis jedis = getConnection()) {
            String jsonStr = jedis.get(optionsKeysNS(pollID));
            List<pollOption> optionlist= fromJsonToOptionList(jsonStr);
            optionlist.add(option);
            jedis.set(optionsKeysNS(pollID), writeToJsonFileOptions(optionlist));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public static List<pollOption> fromJsonToOptionList(String jsonString) throws IOException {
        //create ObjectMapper instance
        ObjectMapper objectMapper = new ObjectMapper();

        //conversion of JSON string to List of Bet object
        return objectMapper.readValue(jsonString, new TypeReference<List<pollOption>>() {
        });
    }
    public void removeOption(Integer pollID, pollOption option) {
        try (Jedis jedis = getConnection()) {
            String jsonStr = jedis.get(optionsKeysNS(pollID));
            List<pollOption> optionList= fromJsonToOptionList(jsonStr);
            if (!optionList.isEmpty()) {
                optionList.remove(option);
                jedis.set(optionsKeysNS(pollID), writeToJsonFileOptions(optionList));
                jedis.expire(optionsKeysNS(pollID), EXPIRATION_IN_SEC);
            } else {
                jedis.del(pollIDRealKeysNS(pollID));
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


    public void updatePollName(Integer pollID, String newPollName) {
        Jedis jedis = getConnection();
        String key = pollNameKeysNS(pollID);
        jedis.set(key , newPollName);
        jedis.expire(key, EXPIRATION_IN_SEC);
        jedis.close();
    }

    public void updatePollType(Integer pollID, String newPollType) {
        Jedis jedis = getConnection();
        String key = pollTypeKeysNS(pollID);
        jedis.set(key , newPollType);
        jedis.expire(key, EXPIRATION_IN_SEC);
        jedis.close();
    }

    public void updateActivationDate(Integer pollID, String newActivationDate) {
        Jedis jedis = getConnection();
        String key = activationDateKeysNS(pollID);
        jedis.set(key , newActivationDate);
        jedis.expire(key, EXPIRATION_IN_SEC);
        jedis.close();
    }
    public void updatePollOptionVotes (Integer pollID, pollOption option, boolean inc) {
        try (Jedis jedis = getConnection()) {
            String jsonStr = jedis.get(optionsKeysNS(pollID));
            List<pollOption> optionList= fromJsonToOptionList(jsonStr);
            if (!optionList.isEmpty()) {
                for (pollOption op : optionList) {
                    if (op.getOptionCaption().equals(option.getOptionCaption())) {
                        if (inc)
                            op.voteOption();
                        else if(op.getOptionVotes() > 0)
                            op.unvoteOption();
                        break;
                    }
                }
                jedis.set(optionsKeysNS(pollID), writeToJsonFileOptions(optionList));
                jedis.expire(optionsKeysNS(pollID), EXPIRATION_IN_SEC);
            } else {
                jedis.del(pollIDRealKeysNS(pollID));
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    public void addPollToMongoDB(Poll poll) {
        PollMongoDBDAO pollMongoDBDAO = new PollMongoDBDAO();
        pollMongoDBDAO.openConnection();
        pollMongoDBDAO.addPoll(poll);
        pollMongoDBDAO.closeConnection();
    }

    //mongoDb
    public void addPoll(Poll poll) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public Integer replacePoll(Document query, Poll newPoll) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public Integer replacePoll(Poll newPoll) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void removePoll(Document query) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void removePoll(Integer pollID) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public List<Poll> getPolls(Document query, Document projection) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }


}
