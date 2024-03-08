package it.unipi.dii.dao.redis;

import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Poll;
import it.unipi.dii.model.Slip;
import it.unipi.dii.model.pollOption;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.Response;
import redis.clients.jedis.Transaction;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {

    public static void main(String[] args) throws IOException {
        /*
        SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
        Slip slip = new Slip("dio", " ","2022-01-01",5);
        slip.setSlipID(3);
        Bet bet = new Bet(1,  4.2 , "1X" , "2022-09-28T07:06:56Z");
        bet.setTeamAway("Juventus");
        bet.setTeamHome("Milan");
        bet.setCompetition_id("SA");
        Bet bet2 = new Bet(2,  5.1, "X2" , "2022-09-28T07:06:56Z");
        bet2.setTeamAway("Napoli");
        bet2.setTeamHome("Inter");
        bet2.setCompetition_id("SA");
        List<Bet> betList = new ArrayList<>();
        betList.add(bet);
        betList.add(bet2);
        slip.setBetsList(betList);
        slipRedisDAO.create_Slip(slip);
        //la crea funziona

         */




/*
        List<Slip> list = slipRedisDAO.getListFromUser("mauro");
        int i = 1;
        for (Slip s : list) {
            if(i != s.getSlipID()){
                System.out.println("primo vuoto:" + i);
                break;
            }
            if(i == list.size()){
                System.out.println("ultimo pieno:" + i);
            }
            i++;

        }
*/

        //slipRedisDAO.sendConfirmedSlipToMongo("mauro",7);


        /*
        //List<Slip> list = slipRedisDAO.getListFromUser("mauro");
        //ObjectMapper objectMapper = new ObjectMapper();

        // Converte la lista di oggetti in una stringa JSON

        //System.out.println(str);
        //la lista funziona

        //slipRedisDAO.delete_Slip("mauro", 3);
        //List<Slip> list = slipRedisDAO.getListFromUser("mauro");
        //ObjectMapper objectMapper = new ObjectMapper();
        //String str = objectMapper.writeValueAsString(list);
        //System.out.println(str);
        //la delete funziona

        //Bet bet4 = new Bet(1,  112.2 , "X");
        //bet4.setTeamAway("Catania");
        //bet4.setTeamHome("Empoli");
        //bet4.setCompetition_id("SA");
        //slipRedisDAO.addBetToSlip("mauro", 8,bet4);
        //la addbet funziona

        //DUBBIO: se aggiungo una bet ad una chiave il suo TTL si aggiorna!?

        //slipRedisDAO.deleteBetFromSlip("mauro", 12, bet);
        //aggiungere hascode e equals a bet
        //la deletebet funziona

        //sendtomongodb da provare con save

        //ObjectMapper objectMapper = new ObjectMapper();
        //String str = objectMapper.writeValueAsString( slipRedisDAO.load("mauro", 121));
        //System.out.println(str);
        //la load funziona


        //slipRedisDAO.refreshTTL("mauro");
        //la refresh funziona
        //attenzione che anche refreshando il ttl non va oltre il valore di refresh

         */

        /*
        PollRedisDAO pollRedisDAO = new PollRedisDAO();
        Poll poll = new Poll("Chi è il miglior attaccante?", "best player","2022-01-01",5,"2021-01-01");
        List<pollOption> pollOptionList = new ArrayList<>();
        pollOption pollOption = new pollOption("Cristiano Ronaldo");
        pollOption pollOption2 = new pollOption("Francesco Taverna");
        pollOption pollOption3 = new pollOption("Gabriele Pianigiani");
        pollOptionList.add(pollOption);
        pollOptionList.add(pollOption2);
        pollOptionList.add(pollOption3);
        poll.setOptions(pollOptionList);
        poll.setActivationDate("2021-01-01");
        poll.setPollID(5);
        pollRedisDAO.addPollToRedis(poll);
        //add poll funziona
        //pollRedisDAO.removePollfromRedis(2);
        //remove poll funziona
        pollRedisDAO.addOption(5, new pollOption("Ronaldo"));
        //add options funziona
        pollRedisDAO.removeOption(5, new pollOption("Ronaldo"));
        //remove poll funziona
        pollRedisDAO.updatePollName(5, "Chi è il miglior portiere?");
        pollRedisDAO.updateActivationDate(5, "2022-01-01");
        pollRedisDAO.updatePollType(5, "best goalkeeper");

        List<Poll> listpoll = pollRedisDAO.getAllPollFromRedis();
        pollRedisDAO.addPollToMongoDB(listpoll.get(0));

        */
    }




}
