package it.unipi.dii.testing;


import static it.unipi.dii.testing.transitionsTests.*;
import static it.unipi.dii.utility.mongoUtility.deactivateMongoDBNotifications;

public class testLauncher {
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