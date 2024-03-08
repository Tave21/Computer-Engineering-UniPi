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

    /**
     * Create a thread that call the update matches function every 60 seconds.
     */
    private static void launchPeriodicUpdate(){
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        UpdateMatchPeriodicThread updateTask = new UpdateMatchPeriodicThread();
        executorService.scheduleAtFixedRate(updateTask, 0, 1, TimeUnit.MINUTES);
    }
}



