//Implement the methods of the UserControllerInterface interface
package it.unipi.dsmt.FleetFra.controller.impl;


import it.unipi.dsmt.FleetFra.DAO.UserDAO;
import it.unipi.dsmt.FleetFra.service.Matchmaking;
import it.unipi.dsmt.FleetFra.service.UserService;
import it.unipi.dsmt.FleetFra.DTO.*;
import it.unipi.dsmt.FleetFra.controller.UserControllerInterface;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.web.bind.annotation.*;
import it.unipi.dsmt.FleetFra.util.SessionManagement;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;


//this class manages HTTP requests and every response is a JSON object
@RestController
public class UserController implements UserControllerInterface {
    //Spring framework will automatically inject the SessionManagement object
    //if there is already a singleton, Spring will use it otherwise it will create a new one
    @Autowired
    SessionManagement session;

    private UserDAO userDao = new UserDAO();

    //endpoint API for signup
    @PostMapping("/signup")
    @Override
    public ResponseEntity<String> signUp(@RequestBody UserDTO UserSignUp) {

        //create a new UserDTO object with the data received from the request
        UserDTO user = new UserDTO(UserSignUp.getFirstName(), UserSignUp.getLastName(), UserSignUp.getUsername(), UserSignUp.getPassword(), UserSignUp.getEmail());
        //call the signup method of the UserDAO class
        int control = userDao.signup(user);

        if (control == 1){
            //set the username of the logged user
            session = SessionManagement.getInstance();
            session.setLogUser(user.getUsername());

            return new ResponseEntity<>("Signup success", HttpStatus.OK);
        }
        else if(control == 0) return new ResponseEntity<>("Username already used", HttpStatus.BAD_REQUEST);

        else return new ResponseEntity<>("User not inserted", HttpStatus.BAD_REQUEST);

    }

    //class to manage the game
    UserService userService = new UserService();
    ConcurrentHashMap<String, Matchmaking> waitingQueue = new ConcurrentHashMap<>();

    //endpoint API for game
    @Async
    @PostMapping("/game")
    @Override
    public ResponseEntity<String> game(@RequestBody String request) {
        System.out.println("Request username: " +  " " + request);

        // Call the handleGame method to handle the request
        ArrayList<String> match = null;
        match = userService.handleGame(request, waitingQueue);


        if (match.getFirst() != null) {
            // A match has been found
            String jsonResult = "{\"matchId\":\"" + match.getFirst() + "\",\"player1\":\"" +
                    request + "\",\"player2\":\"" + match.getLast() + "\"}";
            System.out.println(jsonResult);


            waitingQueue.remove(match.getFirst());

            return new ResponseEntity<>(jsonResult, HttpStatus.OK);
        }

        // Any match found, the player is waiting
        return new ResponseEntity<>("Waiting for opponent...", HttpStatus.OK);
    }


}




