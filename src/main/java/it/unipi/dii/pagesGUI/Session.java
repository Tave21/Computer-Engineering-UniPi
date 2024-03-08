package it.unipi.dii.pagesGUI;

import it.unipi.dii.userCookie.CustomerInfo;

public class Session {
    private static String username;
    private static CustomerInfo customerInfo;

    public static String getUsername() {
        return username;
    }

    public static void setUsername(String username) {
        Session.username = username;
    }

    public static void setCustomerInfo(CustomerInfo customerInfo) {
        Session.customerInfo = customerInfo;
    }
    public static CustomerInfo getCustomerInfo() {
        return customerInfo;
    }
}
