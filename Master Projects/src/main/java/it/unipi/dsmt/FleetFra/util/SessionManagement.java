package it.unipi.dsmt.FleetFra.util;
import org.springframework.stereotype.Component;
import java.util.ArrayList;

//singleton class to manage the session of the users
@Component
public class SessionManagement {

    private static SessionManagement session = null;

    //list of users to track the logged users
    private ArrayList<String> userLogged = new ArrayList<>();

    private SessionManagement() {}

    //singleton pattern: Lazy initialization
    public static SessionManagement getInstance() {
        if(session == null) {
            session = new SessionManagement();
        }
        return session;
    }
    //add the user to the list of logged users
    public void setLogUser(String username) {
        if(session == null) {
            throw new RuntimeException("Session is not active.");
        } else {

            userLogged.add(username);
        }
    }
    //check if the user is logged and return true if it is
    public boolean isUserLogged(String Username) {
        if(session == null) {
            return false;
        } else {
            return userLogged.contains(Username);
        }
    }
    //remove the user from the list of logged users
    public boolean logoutUser(String Username){
        if(isUserLogged(Username)){
            userLogged.remove(Username);
            return true;
        }
        return false;
    }


}
