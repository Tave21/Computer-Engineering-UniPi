package it.unipi.dii.pagesGUI;
import it.unipi.dii.model.customerInfo;
public class Session {
    private static String username;
    private static customerInfo customerInfo;
    public static String getUsername() {
        return Session.username;
    }
    public static void setUsername(String username) {
        Session.username = username;
    }

    public static void setCustomerInfo(customerInfo customerInfo) {
        Session.customerInfo = customerInfo;
    }
    public static customerInfo getCustomerInfo() {
        return Session.customerInfo;
    }
}
