package it.unipi.dsmt.FleetFra.controller;

import it.unipi.dsmt.FleetFra.DTO.DateDTO;
import it.unipi.dsmt.FleetFra.DTO.ViewUsersRequestDTO;
import org.springframework.http.ResponseEntity;

public interface AdminControllerInterface {

    ResponseEntity<String> viewUsers(ViewUsersRequestDTO request);

    ResponseEntity<String> removeUser(String username);

    ResponseEntity<String> viewRemoveUser(ViewUsersRequestDTO request);

    ResponseEntity<String> browseGames(DateDTO request);

}