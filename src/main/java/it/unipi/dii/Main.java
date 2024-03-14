package it.unipi.dii;


import static it.unipi.dii.testing.matchesTransitionTests.transitionsTests.*;
import static it.unipi.dii.utility.MongoUtility.deactivateMongoDBNotifications;

public class Main {
    public static void main(String[] args) {
        deactivateMongoDBNotifications();
        testTIMEDtoCANCELED();
        testTIMEDtoINPLAYtoPAUSEDtoINPLAYtoFINISHED();
        testTIMEDtoPOSTPONED();

        System.out.println("OK!");
    }
}