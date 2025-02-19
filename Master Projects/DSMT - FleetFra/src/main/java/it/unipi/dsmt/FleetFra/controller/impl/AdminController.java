//Implement the methods of the AdminControllerInterface interface
package it.unipi.dsmt.FleetFra.controller.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dsmt.FleetFra.DAO.*;

import it.unipi.dsmt.FleetFra.controller.AdminControllerInterface;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import it.unipi.dsmt.FleetFra.DTO.*;

import java.util.List;


//this class manages http requests and every response is a json object
@RestController
public class AdminController implements AdminControllerInterface {

    //DAO object for database user interaction
    private UserDAO userDAO = new UserDAO();

    //endpoint API for user visualization
    @PostMapping("/viewDetailedUsers")
    @Override
    public ResponseEntity<String> viewUsers(@RequestBody ViewUsersRequestDTO request) {
        //list of users to be displayed with stats
        List<UserDTO> list = userDAO.viewUsers(request, true);
        //ObjectMapper to convert the list of users into a JSON object
        ObjectMapper objectMapper = new ObjectMapper();

        try {
            String jsonResult = objectMapper.writeValueAsString(list);
            //return HTTP response with JSON object with Code 200
            return new ResponseEntity<>(jsonResult, HttpStatus.OK);

        } catch (JsonProcessingException e) {
            e.printStackTrace();
            return new ResponseEntity<>("Error during JSON serialization", HttpStatus.BAD_REQUEST);
        }
    }

    //endpoint API for user removal
    @PostMapping("/removeUser")
    @Override
    public ResponseEntity<String> removeUser(@RequestBody String username) {
        //remove the user from the database
        if(!userDAO.removeUser(username)){
            return new ResponseEntity<>("Error during the operation", HttpStatus.BAD_REQUEST);
        }

        return new ResponseEntity<>("User '"+username+"' removed", HttpStatus.OK);
    }

    //endpoint API for user removal
    @PostMapping("/viewUsers")
    @Override
    public ResponseEntity<String> viewRemoveUser(@RequestBody ViewUsersRequestDTO request) {
        //list of users to be displayed without stats, because it is the user removal page
        List<UserDTO> list = userDAO.viewUsers(request, false);
        //ObjectMapper to convert the list of users into a JSON object
        ObjectMapper objectMapper = new ObjectMapper();

        try {
            String jsonResult = objectMapper.writeValueAsString(list);
            //return HTTP response with JSON object with Code 200
            return new ResponseEntity<>(jsonResult, HttpStatus.OK);

        } catch (JsonProcessingException e) {
            e.printStackTrace();
            return new ResponseEntity<>("Error during JSON serialization", HttpStatus.BAD_REQUEST);
        }
    }

    //endpoint API for game visualization
    @PostMapping("/browseGamesAdmin")
    @Override
    // return a list of games
    public ResponseEntity<String> browseGames( @RequestBody DateDTO request) {
        //DAO object for database match interaction
        MatchDAO matchDAO = new MatchDAO();
        ObjectMapper objectMapper = new ObjectMapper();

        //get the list of games played by the users
        List<MatchDTO> list = matchDAO.browseGames(request.getDate1(), request.getDate2());

        try {
            //convert the list of games into a JSON object
            String jsonResult = objectMapper.writeValueAsString(list);
            //return HTTP response with JSON object with Code 200
            return new ResponseEntity<>(jsonResult, HttpStatus.OK);

        } catch (JsonProcessingException e) {
            e.printStackTrace();
            return new ResponseEntity<>("Error during JSON serialization", HttpStatus.BAD_REQUEST);
        }
    }

}

