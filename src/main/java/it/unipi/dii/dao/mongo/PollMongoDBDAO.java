package it.unipi.dii.dao.mongo;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCursor;
import it.unipi.dii.dao.PollDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import it.unipi.dii.model.Poll;
import it.unipi.dii.model.pollOption;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.dateTimes.getCurrentDate;
import static it.unipi.dii.utility.converters.jsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.mongoUtility.deleteDocuments;
import static it.unipi.dii.utility.mongoUtility.insertDocuments;
import static it.unipi.dii.utility.converters.objectToDocumentConverter.ObjectToDocumentConverter;
import static it.unipi.dii.utility.converters.objectToJsonStringConverter.convertObjectToJsonString;

public class PollMongoDBDAO extends BaseMongoDAO implements PollDAO {
    /**
     * Add a new poll in the database MongoDB.
     * @param poll The poll object to add.
     */
    @Override
    public void addPoll(Poll poll) {
        poll.setPollID(this.getLastID() + 1);
        List<Document> documents = new ArrayList<>();
        documents.add(Document.parse(convertObjectToJsonString(poll)));
        insertDocuments(this.mongoDB.getCollection("polls") , documents);
    }

    /**
     * Fetch the biggest value of pollID from MongoDB.
     * @return The biggest value of pollID, -1 if the collection is empty.
     */
    public Integer getLastID() {
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("activationDate",
                                new Document("$gt", getCurrentDate().minusYears(2).toString()))),
                new Document("$project",
                        new Document("pollID", 1L)
                                .append("_id", 0L)),
                new Document("$sort",
                        new Document("pollID", -1L)),
                new Document("$limit", 1L));
        AggregateIterable<Document> result = this.mongoDB.getCollection("polls").aggregate(pipeline);
        try {
            return Objects.requireNonNull(result.first()).getInteger("pollID");
        }catch(NullPointerException e){
            return -1; // The poll collection is empty.
        }
    }

    /**
     * Replace the first poll that match the criteria with the info of newPoll.
     * @param query Match criteria.
     * @param newPoll The poll information to be inserted.
     * @return The poll ID.
     */
    @Override
    public Integer replacePoll(Document query ,Poll newPoll) {
        this.mongoDB.getCollection("polls").replaceOne(query, ObjectToDocumentConverter(newPoll));
        return newPoll.getPollID();
    }
    /**
     * Replace the first poll that have the same PollID of newPoll.
     * @param newPoll The poll information to be inserted.
     * @return The poll id.
     */
    @Override
    public Integer replacePoll(Poll newPoll) {
        return replacePoll(new Document("pollID" , new Document("$eq",  newPoll.getPollID())) , newPoll);
    }

    /**
     * Remove all the match that respect the criteria.
     * @param query Match criteria.
     */
    @Override
    public void removePoll(Document query) {
        deleteDocuments(this.mongoDB.getCollection("polls") , query);
    }

    /**
     * Remove the poll with the target pollID.
     * @param pollID The id of the poll.
     */

    @Override
    public void removePoll(Integer pollID) {
        removePoll(new Document("pollID" , new Document("$eq",  pollID)));
    }

    /**
     *
     * @param query Match criteria.
     * @param projection Projection criteria.
     * @return The poll list that match the criteria.
     */

    @Override
    public List<Poll> getPolls(Document query, Document projection) {
        List<Poll> s_list = new ArrayList<>();
        try (MongoCursor<Document> cursor = this.mongoDB.getCollection("polls").find(query).projection(projection).iterator()) {
            while (cursor.hasNext()) {
                Document document = cursor.next();
                Poll s = convertJsonToObject(document.toJson(), Poll.class);
                s_list.add(s);
            }
        }
        return s_list;
    }

    // Redis
    public void refreshTTL(String username) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }
    public void createPollCookieOfUser(String username, String cookie) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }
    public String getPollCookieOfUser(String username) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }
    public void addPollToRedis(Poll poll) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void removePollfromRedis(Integer pollID) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void addOption(Integer pollID, pollOption option) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void addPollToMongoDB(Poll poll) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void removeOption(Integer pollID, pollOption option) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void updatePollName(Integer pollID, String newPollName) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void updateActivationDate(Integer pollID, String newActivationDate) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void updatePollType(Integer pollID, String newPollType) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public List<Poll> getAllPollFromRedis() {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

    public void updatePollOptionVotes (Integer pollID, pollOption option, boolean inc) {
        throw new
                UnsupportedOperationException("Not supported in MongoDB implementation");
    }

}
