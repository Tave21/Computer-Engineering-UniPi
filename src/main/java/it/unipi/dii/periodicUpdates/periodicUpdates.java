package it.unipi.dii.periodicUpdates;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static it.unipi.dii.utility.MongoUtility.deactivateMongoDBNotifications;
public class periodicUpdates {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();
        launchPeriodicUpdate(); // launch of the update function.
    }

    private static void launchPeriodicUpdate(){
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();

        // Every minute, the system checks if there are any matches updates.
        UpdateMatchPeriodicThread updateMatchTask = new UpdateMatchPeriodicThread();
        executorService.scheduleAtFixedRate(updateMatchTask, 0, 1, TimeUnit.MINUTES);

        // Every 5 hours, the system checks if some poll has to be deleted from Redis.
        UpdatePollPeriodicThread updatePollTask = new UpdatePollPeriodicThread();
        executorService.scheduleAtFixedRate(updatePollTask, 0, 1, TimeUnit.HOURS);
    }
}



