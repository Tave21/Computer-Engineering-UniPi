package it.unipi.dii.pagesGUI;

public class Session {
    private static String username;
    private static it.unipi.dii.userCookie.customerInfo customerInfo;

    public static String getUsername() {
        return username;
    }

    public static void setUsername(String username) {
        Session.username = username;
    }

    public static void setCustomerInfo(it.unipi.dii.userCookie.customerInfo customerInfo) {
        Session.customerInfo = customerInfo;
    }
    public static it.unipi.dii.userCookie.customerInfo getCustomerInfo() {
        return customerInfo;
    }
}
