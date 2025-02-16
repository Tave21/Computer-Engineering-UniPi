package it.unipi.dii.dao;

import com.mongodb.client.AggregateIterable;
import it.unipi.dii.model.Customer;
import it.unipi.dii.model.Slip;
import org.bson.Document;

import java.util.List;

public interface StatisticsDAO {
    List<String> showUsersFavouriteTeams(String dateFrom, String dateTo, int howManyShow);
    double showFinancialResults(String dateFrom, String dateTo);
    List<String> showMostAppreciatedTeamsPolls();
    List<String> showMostAppreciatedPlayersPolls();
    AggregateIterable<Document> averageNumberOfMatchesForEachChampionshipForEachSlip(String dateFrom, String dateTo);
}
