package it.unipi.dii.analyticsPeriodicCalculator;

import com.mongodb.client.model.ReplaceOptions;
import it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.financialResults.financialReport;
import it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.financialResults.financialValue;
import it.unipi.dii.dao.mongo.StatisticsMongoDBDAO;
import org.bson.Document;
import java.util.TimerTask;
import static it.unipi.dii.utility.converters.jsonToDocumentConverter.convertJsonToDocument;
import static it.unipi.dii.utility.dateTimes.*;
import static it.unipi.dii.utility.converters.objectToJsonStringConverter.convertObjectToJsonString;

public class updateHourlyAnalytics extends TimerTask {
    /**
     * Script that launches the periodic update of the light analytics in MongoDB.
     */
    @Override
    public void run() {
        System.out.println("Ok Hour start");
        financialResultHourlyCalculator();
        System.out.println("Ok Hour end");
    }

    public static void financialResultHourlyCalculator(){
        StatisticsMongoDBDAO st = new StatisticsMongoDBDAO();
        st.openConnection();

        financialReport report = new financialReport();

        report.setPeriodRelated(getFirstDayOfMonth().toString());
        report.setComputationTimestamp(getCurrentInstantString());
        report.setType("financial");

        financialValue financialVal = new financialValue();
        financialVal.setDateFrom(report.getPeriodRelated());
        financialVal.setDateTo(getCurrentDate().plusDays(1).toString());
        financialVal.setValue(st.showFinancialResults(
                financialVal.getDateFrom() ,
                financialVal.getDateTo()
        ));
        report.addValueToList(financialVal);



        st.mongoDB.getCollection("analytics").replaceOne(
                new Document("periodRelated",  report.getPeriodRelated()).append("type" , report.getType()),
                convertJsonToDocument(convertObjectToJsonString(report)),
                new ReplaceOptions().upsert(true)
        );
        st.closeConnection();
    }
}