package it.unipi.dii;

import com.mongodb.client.MongoCursor;
import it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.seventhQuery.mainReport;
import it.unipi.dii.dao.mongo.StatisticsMongoDBDAO;
import org.bson.Document;

import static it.unipi.dii.utility.converters.jsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.dateTimes.getFirstDayOfMonth;
import static it.unipi.dii.utility.mongoUtility.deactivateMongoDBNotifications;

public class Main {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();
        StatisticsMongoDBDAO st = new StatisticsMongoDBDAO();
        st.openConnection();

        mainReport m;
        try (MongoCursor<Document> cursor = st.mongoDB.getCollection("analytics")
                .find(new Document("type", "seventh").append("periodRelated", getFirstDayOfMonth().toString()))
                .projection(new Document("_id", 0L))
                .iterator()) {
            Document document = cursor.next();
            m = convertJsonToObject(document.toJson(), mainReport.class);
            assert m != null;
            System.out.println(m.toString());
        } catch (NullPointerException e) {
            System.out.println("Nada");
        }

        st.closeConnection();

    }
}
