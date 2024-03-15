package it.unipi.dii;


import static it.unipi.dii.testing.transitionsTests.*;
import static it.unipi.dii.utility.MongoUtility.deactivateMongoDBNotifications;

public class Main {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();
        testTIMEDtoCANCELED();
        testTIMEDtoINPLAYtoPAUSEDtoINPLAYtoFINISHED();
        testTIMEDtoPOSTPONED();
        testCustomers();
        testPolls();
        System.out.println("OK! All test succeeded, BeansBet works perfectly!");
    }
}