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
     * Script that launches the periodic update of the matches in MongoDB.
     */
    @Override
    public void run() {
        financialResultHourlyCalculator();
    }

    public static void financialResultHourlyCalculator(){
        StatisticsMongoDBDAO st = new StatisticsMongoDBDAO();
        st.openConnection();

        financialReport report = new financialReport();

        report.setPeriodRelated(getFirstDayOfMonth().toString());
        report.setComputationTimestamp(getCurrentInstantString());
        report.setType("financial");

        financialValue financialVal = new financialValue();
        financialVal.setDateTo(report.getPeriodRelated());
        financialVal.setDateFrom(getCurrentDateString());
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
