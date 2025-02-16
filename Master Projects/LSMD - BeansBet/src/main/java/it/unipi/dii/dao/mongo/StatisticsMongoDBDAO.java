package it.unipi.dii.dao.mongo;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import it.unipi.dii.dao.StatisticsDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import org.bson.BsonNull;
import org.bson.Document;
import java.util.*;
import static it.unipi.dii.utility.generators.randomGeneration.truncateNumber;

public class StatisticsMongoDBDAO extends BaseMongoDAO implements StatisticsDAO {

    @Override
    public List<String> showUsersFavouriteTeams(String dateFrom, String dateTo, int howManyShow) {

        List<Document> PipelineHome = Arrays.asList(new Document("$match",
                        new Document("confirmationDate",
                                new Document("$gt", dateFrom)
                                        .append("$lt", dateTo))),
                new Document("$unwind",
                        new Document("path", "$betsList")),
                new Document("$project",
                        new Document("comp", "$betsList.teamHome")
                                .append("slip", "$slipID")
                                .append("_id", 0L)),
                new Document("$group",
                        new Document("_id", "$comp")
                                .append("howMany",
                                        new Document("$sum", 1L))),
                new Document("$sort",
                        new Document("howMany", -1L)));

        List<Document> PipelineAway = Arrays.asList(new Document("$match",
                        new Document("confirmationDate",
                                new Document("$gt", dateFrom)
                                        .append("$lt", dateTo))),
                new Document("$unwind",
                        new Document("path", "$betsList")),
                new Document("$project",
                        new Document("comp", "$betsList.teamAway")
                                .append("slip", "$slipID")
                                .append("_id", 0L)),
                new Document("$group",
                        new Document("_id", "$comp")
                                .append("howMany",
                                        new Document("$sum", 1L))),
                new Document("$sort",
                        new Document("howMany", -1L)));


        MongoCollection<Document> slips_coll = this.mongoDB.getCollection("slips");
        AggregateIterable<Document> docHome = slips_coll.aggregate(PipelineHome);
        AggregateIterable<Document> docAway = slips_coll.aggregate(PipelineAway);

        List<String> doc = new ArrayList<>();
        int counter = 0;

        for (Document document : docHome) {
            long howManyValue = document.getLong("howMany");

            for (Document documentA : docAway) {

                if (Objects.equals(document.getString("_id"), documentA.getString("_id"))) {
                    howManyValue = howManyValue + document.getLong("howMany");
                }
            }
            document.put("howMany", howManyValue);
            doc.add(document.getString("_id"));
            counter++;

            if (counter == howManyShow) {
                break;
            }
        }

        return doc;
    }
    @Override
    public double showFinancialResults(String dateFrom, String dateTo) {
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("confirmationDate",
                                new Document("$gt", dateFrom)
                                        .append("$lt", dateTo))),
                new Document("$project",
                        new Document("_id", 0L)
                                .append("difference",
                                        new Document("$subtract", Arrays.asList("$betAmount", "$amount")))),
                new Document("$group",
                        new Document("_id",
                                new BsonNull())
                                .append("totalDifference",
                                        new Document("$sum", "$difference"))),
                new Document("$project",
                        new Document("_id", 0L)));

        AggregateIterable<Document> docs = this.mongoDB.getCollection("slips").aggregate(pipeline);
        try {
            return truncateNumber(Objects.requireNonNull(docs.first()).getDouble("totalDifference"), 2);
        }catch(NullPointerException e){
            return 0; // No slips have been made in this period.
        }
    }

    @Override
    public List<String> showMostAppreciatedTeamsPolls() {
        // I create the pipeline.
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("pollType", "Best Team")),
                new Document("$unwind",
                        new Document("path", "$options")),
                new Document("$project",
                        new Document("team", "$options.optionCaption")
                                .append("vote", "$options.optionVotes")
                                .append("_id", 0L)),
                new Document("$group",
                        new Document("_id", "$team")
                                .append("howMany",
                                        new Document("$sum", "$vote"))),
                new Document("$sort",
                        new Document("howMany", -1L)),
                new Document("$limit", 10L));

        AggregateIterable<Document> docHome = this.mongoDB.getCollection("polls").aggregate(pipeline);
        List<String> doc = new ArrayList<>();

        for (Document document : docHome) {
            doc.add(document.getString("_id")); // Retrieve the teams' names.
        }

        return doc;
    }

    @Override
    public List<String> showMostAppreciatedPlayersPolls() {
        // I create the pipeline.
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("pollType", "Best Player")),
                new Document("$unwind",
                        new Document("path", "$options")),
                new Document("$project",
                        new Document("player", "$options.optionCaption")
                                .append("vote", "$options.optionVotes")
                                .append("_id", 0L)),
                new Document("$group",
                        new Document("_id", "$player")
                                .append("howMany",
                                        new Document("$sum", "$vote"))),
                new Document("$sort",
                        new Document("howMany", -1L)),
                new Document("$limit", 10L));

        AggregateIterable<Document> docHome = this.mongoDB.getCollection("polls").aggregate(pipeline);
        List<String> doc = new ArrayList<>();

        for (Document document : docHome) {
            doc.add(document.getString("_id")); // Retrieve the players names.
        }

        return doc;
    }

    @Override
    public AggregateIterable<Document> averageNumberOfMatchesForEachChampionshipForEachSlip(String dateFrom, String dateTo) {
        List<Document> Pipeline = Arrays.asList(new Document("$match",
                        new Document("confirmationDate",
                                new Document("$gt", dateFrom)
                                        .append("$lt", dateTo))),
                new Document("$unwind",
                        new Document("path", "$betsList")),
                new Document("$project",
                        new Document("comp", "$betsList.competition_id")
                                .append("slip", "$slipID")
                                .append("_id", 0L)),
                new Document("$group",
                        new Document("_id",
                                new Document("comp", "$comp")
                                        .append("slip", "$slip"))
                                .append("cont",
                                        new Document("$sum", 1L))),
                new Document("$group",
                        new Document("_id", "$_id.comp")
                                .append("HowMany",
                                        new Document("$avg", "$cont"))));
        return this.mongoDB.getCollection("slips").aggregate(Pipeline);
    }



}
