package it.unipi.dii.dao;

import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Slip;
import org.bson.Document;

import java.util.List;

 public interface SlipDAO{
     // Mongo
    Integer addSlip(Slip slip);
    void removeSlip(Document query);
    void replaceSlip(Integer slipID , Slip slip);
    void removeBet(String slipId, Integer matchID);
    void removeAllBetsOfMatch(Integer matchID);
    List<Slip> getSlips(Document query, Document projection);
    Slip getSlip(Integer slipID);

    //Redis

     public List<String> getAllUsernames() ;
     void deleteBetFromSlip(String username, Integer slipID, Bet bet);

     String slipBetsKeysNS(String username, Integer slipID);

     String slipbetAmountKeysNS(String username, Integer slipID);

     String slipcreationDateKeysNS(String username, Integer slipID);

     String UsernameKeysNS(String username);

     String slipKeysNS(String username, Integer slipID);

     void persist(Slip slip); //renew the expiration time of a specific slip

     Slip load(String username, Integer slipID , double betAmount); //gets bets from a specific slip

     int create_Slip(Slip slip); //data una slip, crea una chiave e la inserisce nel database

     void refreshTTL(String userID,String slipID); //refresh the expiration time of a specific slip

     void delete_Slip(String username, Integer slipID);  //delete slip from redis

     int addBetToSlip(String username, Integer slipID, Bet bet);

     void sendConfirmedSlipToMongo(String username, Integer slipID , double betAmount);



}
