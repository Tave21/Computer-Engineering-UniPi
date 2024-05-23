package it.unipi.dii.dao;

import it.unipi.dii.model.Poll;
import it.unipi.dii.model.pollOption;
import org.bson.Document;

import java.io.IOException;
import java.util.List;

public interface PollDAO {
    void addPoll(Poll poll);

    Integer replacePoll(Document query, Poll newPoll);

    Integer replacePoll(Poll newPoll);

    void removePoll(Document query);

    void removePoll(Integer pollID);

    List<Poll> getPolls(Document query, Document projection);


    //Redis
    String getPollCookieOfUser(String username);

    void createPollCookieOfUser(String username, String cookie) ;
    public void refreshTTL(String username) ;
    void addPollToRedis(Poll poll);

    void removePollfromRedis(Integer pollID);
    List<Poll> getAllPollFromRedis() ;

    void addOption(Integer pollID, pollOption option);

    void removeOption(Integer pollID, pollOption option);

    void addPollToMongoDB(Poll poll);

    void updatePollName(Integer pollID, String newPollName);

    void updateActivationDate(Integer pollID, String newActivationDate);

    void updatePollType(Integer pollID, String newPollType);

    void updatePollOptionVotes (Integer pollID, pollOption option, boolean inc) ;
}
