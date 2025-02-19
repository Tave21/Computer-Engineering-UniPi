//Implement the methods of the GenericControllerInterface interface
package it.unipi.dsmt.FleetFra.controller.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dsmt.FleetFra.DAO.*;
import it.unipi.dsmt.FleetFra.DTO.*;
import it.unipi.dsmt.FleetFra.controller.GenericControllerInterface;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import it.unipi.dsmt.FleetFra.util.SessionManagement;

import java.util.List;

//this class manages http requests and every response is a json object
@RestController
//this annotation is used to store the user logged in the session
@SessionAttributes("userlog")
public class GenericController implements GenericControllerInterface {
    //Spring framework will automatically inject the SessionManagement object
    //if there is already a singleton, Spring will use it otherwise it will create a new one
    @Autowired
    SessionManagement session;

    private UserDAO userDao = new UserDAO();

    //API for the login of a user
    @PostMapping("/login")
    @Override
    public ResponseEntity<ResponseRequest> login(@RequestBody UserAccessRequest requestUser) {
        //get the session
        session = SessionManagement.getInstance();
        //check if the user is already logged and return an error
        if(session.isUserLogged(requestUser.getUsername()))
            return new ResponseEntity<>(new ResponseRequest("User already logged"), HttpStatus.FORBIDDEN);

        ResponseRequest responsereq;

        try {
            //verify the credentials
            UserDTO user = userDao.login(requestUser.getUsername(), requestUser.getPassword());
            //if the user is not found or credentials are incorrect, return an error
            if (user == null) {
                responsereq = new ResponseRequest("Wrong Username or Password inserted");
                System.out.println("Login failed");
                return new ResponseEntity<>(responsereq, HttpStatus.BAD_REQUEST);
            }

            //add the user to the list of logged users
            session.setLogUser(user.getUsername());
        } catch (Exception e) {
            responsereq = new ResponseRequest(e.getMessage());
            return new ResponseEntity<>(responsereq, HttpStatus.UNAUTHORIZED);
        }

        ResponseRequest response = new ResponseRequest("Login success");
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    //API for log check
    @PostMapping("/isLogged")
    @Override
    public ResponseEntity<String> isLogged(@RequestBody String Username) {
        session = SessionManagement.getInstance();
        //check if the user is logged
        if (!session.isUserLogged(Username))
            return new ResponseEntity<>(HttpStatus.FORBIDDEN);
        else
            return new ResponseEntity<>("OK", HttpStatus.OK);
    }

    //API for the logout
    @PostMapping("/logout")
    @Override
    public ResponseEntity<String> logout(@RequestBody String Username) {
        session = SessionManagement.getInstance();
        //check if the user is logged
        if(!session.isUserLogged(Username)){
            ResponseRequest response = new ResponseRequest("Logout failed");
            return new ResponseEntity<>(response.getAnswer(), HttpStatus.FORBIDDEN);
        }
        //remove the user from the list of logged users
        session.logoutUser(Username);
        ResponseRequest response = new ResponseRequest("Logout success");
        return new ResponseEntity<>(response.getAnswer(), HttpStatus.OK);
    }

    //API for browsing games
    @PostMapping("/browseGames")
    @Override
    public ResponseEntity<String> browseGames(@RequestBody String username) {
        MatchDAO matchDAO = new MatchDAO();
        //ObjectMapper is used to serialize the object in a json object
        ObjectMapper objectMapper = new ObjectMapper();

        //get the list of games played by the user
        List<MatchDTO> list = matchDAO.browseGames("data1", "data2");

        try {
            String jsonResult = objectMapper.writeValueAsString(list);
            return new ResponseEntity<>(jsonResult, HttpStatus.OK);

        } catch (JsonProcessingException e) {
            e.printStackTrace();
            return new ResponseEntity<>("Error during JSON serialization", HttpStatus.BAD_REQUEST);
        }
    }


    //API for inserting a new match
    @PostMapping("/insertMatch")
    @Override
    public ResponseEntity<String> insertMatch(@RequestBody MatchDTO match) {
        MatchDAO matchDAO = new MatchDAO();

        try {
            //insert the match in the database
            if (!matchDAO.insert(match)) {
                return new ResponseEntity<>("Error during the insert", HttpStatus.BAD_REQUEST);
            }

            return new ResponseEntity<>("Inserted correctly", HttpStatus.OK);

        } catch (Exception e) {
            return new ResponseEntity<>("Server error: " + e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }


}