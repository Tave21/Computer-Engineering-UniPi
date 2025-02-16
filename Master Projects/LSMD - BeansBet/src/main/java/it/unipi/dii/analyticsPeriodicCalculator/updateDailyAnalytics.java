package it.unipi.dii.analyticsPeriodicCalculator;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.model.ReplaceOptions;
import it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.seventhQuery.championshipValue;
import it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.seventhQuery.mainReport;
import it.unipi.dii.dao.mongo.StatisticsMongoDBDAO;
import org.bson.Document;

import java.util.List;
import java.util.TimerTask;
import static it.unipi.dii.utility.converters.jsonToDocumentConverter.convertJsonToDocument;
import static it.unipi.dii.utility.converters.objectToJsonStringConverter.convertObjectToJsonString;
import static it.unipi.dii.utility.dateTimes.*;

public class updateDailyAnalytics extends TimerTask {

    /**
     * Script that launches the periodic update of the heavy analytics in MongoDB.
     */
    @Override
    public void run() {
        System.out.println("Ok Day Start");
        averageNumberOfMatchesForEachChampionshipForEachSlipCalculator();
        showUsersFavouriteTeamsCalculator();
        System.out.println("Ok Day end");
    }

    public static void averageNumberOfMatchesForEachChampionshipForEachSlipCalculator() {
        StatisticsMongoDBDAO st = new StatisticsMongoDBDAO();
        st.openConnection();

        mainReport report = new mainReport();

        report.setPeriodRelated(getFirstDayOfMonth().toString());
        report.setComputationTimestamp(getCurrentInstantString());
        report.setType("seventh");

        AggregateIterable<Document> av = st.averageNumberOfMatchesForEachChampionshipForEachSlip(
                report.getPeriodRelated(),
                getCurrentDate().plusDays(1).toString()
        );

        for (Document document : av) {
            report.addValueToList(new championshipValue(
                    document.getString("_id"),
                    document.getDouble("HowMany")
            ));
        }

        st.mongoDB.getCollection("analytics").replaceOne(
                new Document("periodRelated", report.getPeriodRelated()).append("type", report.getType()),
                convertJsonToDocument(convertObjectToJsonString(report)),
                new ReplaceOptions().upsert(true)
        );
        st.closeConnection();
    }

    public static void showUsersFavouriteTeamsCalculator() {
        StatisticsMongoDBDAO st = new StatisticsMongoDBDAO();
        st.openConnection();

        mainReport report = new mainReport();

        report.setPeriodRelated(getFirstDayOfMonth().toString());
        report.setComputationTimestamp(getCurrentInstantString());
        report.setType("users favourite teams");

        List<String> sList = st.showUsersFavouriteTeams(
                report.getPeriodRelated(),
                getCurrentDate().plusDays(1).toString(),
                10
        );

        for (String s : sList) {
            report.addValueToList(new championshipValue(s, 0));
        }

        st.mongoDB.getCollection("analytics").replaceOne(
                new Document("periodRelated", report.getPeriodRelated()).append("type", report.getType()),
                convertJsonToDocument(convertObjectToJsonString(report)),
                new ReplaceOptions().upsert(true)
        );
        st.closeConnection();
    }
}
