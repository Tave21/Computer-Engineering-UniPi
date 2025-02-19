package it.unipi.dsmt.FleetFra.service;


import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class UserService {
    private static final AtomicLong counter = new AtomicLong(0);


    public static String generateUniqueId() {
        // Combine the timestamp with an atomic counter to generate a unique ID
        long timestamp = System.currentTimeMillis();
        long count = counter.getAndIncrement();
        return timestamp + "-" + count;
    }
    public ArrayList<String> handleGame(String request, ConcurrentHashMap<String, Matchmaking> waitingQueue) {
        // Check if there is already a player waiting in the queue
        Matchmaking match;
        boolean matchFound;
        matchFound = !waitingQueue.isEmpty();

        if (matchFound) {
            // Match the requester with the waiting player
            //every access to the waitingQueue is synchronized
            match = waitingQueue.values().iterator().next();

            match.player2 = request;

            // Notify the thread associated with the first player
            synchronized (match) {
                match.notify();
            }

            // Return the match details
            ArrayList<String> ret = new ArrayList<>();
            ret.add(match.matchId);
            ret.add(match.player1);

            return ret;

        } else {
            // If there is no one waiting, add the requester to the queue
            String matchId = generateUniqueId();
            waitingQueue.put(matchId, new Matchmaking(request, null, matchId));
            match = waitingQueue.get(matchId);

            System.out.println("Added to waiting queue: " + request);


            // Wait for the second player to arrive and start the game
            synchronized (match) {
                        try {
                            match.wait();
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
           }


            ArrayList<String> ret = new ArrayList<>();
            ret.add(match.matchId);
            ret.add(match.player2);

            try {
                Thread.sleep(500); // Half of a second of delay.
            }catch (InterruptedException e){
                Thread.currentThread().interrupt();
            }

            return ret; // No match found yet, still waiting
        }
    }


}
