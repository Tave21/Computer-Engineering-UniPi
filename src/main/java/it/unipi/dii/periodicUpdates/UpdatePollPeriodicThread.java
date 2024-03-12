package it.unipi.dii.periodicUpdates;

import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.dao.redis.PollRedisDAO;
import it.unipi.dii.model.Poll;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.TimerTask;

import static it.unipi.dii.utility.DateTimes.getCurrentInstant;

public class UpdatePollPeriodicThread extends TimerTask {
    /**
     * Script that launches the periodic update of the polls in Redis.
     */
    @Override
    public void run() {
        PollRedisDAO pRedis = new PollRedisDAO();
        List<Poll> polllist = pRedis.getAllPollFromRedis();
        for (int i = 0; i < polllist.size(); i++) {
            int id = polllist.get(i).getPollID();

            Instant now = getCurrentInstant();
            Instant activationDate = Instant.parse(polllist.get(i).getActivationDate());
            Instant activationDatePlusOneDay = activationDate.plus(Duration.ofDays(1));


            if(activationDatePlusOneDay.isBefore(now)){
                // We have to remove poll if it is expired,
                // A poll is expired one day after its activation.
                PollRedisDAO pRedis2 = new PollRedisDAO();
                pRedis2.addPollToMongoDB(polllist.get(i)); // Send the poll to MongoDB.
                // Remove the poll from redis if it is elapsed.
                pRedis2.removePollfromRedis(id);
            }
        }

    }
}
