package it.unipi.dii.pagesGUI;
import it.unipi.dii.userCookie.customerInfo;
public class Session {
    private static String username;
    private static customerInfo customerInfo;

    public static String getUsername() {
        return username;
    }

    public static void setUsername(String username) {
        Session.username = username;
    }

    public static void setCustomerInfo(customerInfo customerInfo) {
        Session.customerInfo = customerInfo;
    }
    public static customerInfo getCustomerInfo() {
        return customerInfo;
    }
}
