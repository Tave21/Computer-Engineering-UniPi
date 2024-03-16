package it.unipi.dii.analyticsPeriodicCalculator;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static it.unipi.dii.analyticsPeriodicCalculator.updateDailyAnalytics.*;
import static it.unipi.dii.analyticsPeriodicCalculator.updateHourlyAnalytics.*;
import static it.unipi.dii.utility.mongoUtility.deactivateMongoDBNotifications;

public class periodicAnalytics {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();
        //launchPeriodicAnalytics(); // Launch of the update function.
        //financialResultHourlyCalculator();
        //averageNumberOfMatchesForEachChampionshipForEachSlipCalculator();
        showUsersFavouriteTeamsCalculator();
    }

    private static void launchPeriodicAnalytics(){
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();

        updateDailyAnalytics updateDailyTask = new updateDailyAnalytics();
        executorService.scheduleAtFixedRate(updateDailyTask, 0, 1, TimeUnit.DAYS);

        updateHourlyAnalytics updateHourTask = new updateHourlyAnalytics();
        executorService.scheduleAtFixedRate(updateHourTask, 0, 1, TimeUnit.HOURS);
    }
}



