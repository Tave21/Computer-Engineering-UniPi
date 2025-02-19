package it.unipi.dsmt.FleetFra.DTO;

import com.fasterxml.jackson.annotation.JsonInclude;

//null values (in class attributes) are not included in the json object
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserDTO {
    private String firstName;
    private String lastName;
    private String username;
    private String password;
    private String email;
    private int playedGames;
    private int lostGames;
    private int winGames;

    public UserDTO(){}

    //constructor
    public UserDTO(String firstName, String lastName, String username, String password, String email) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.username= username;
        this.password = password;
        this.email = email;
    }
    //getters and setters
    public String getFirstName() {
        return firstName;
    }
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }
    public String getLastName() {
        return lastName;
    }
    public void setLastName(String lastName) {
        this.lastName = lastName;
    }
    public String getUsername() {
        return username;
    }
    public void setUsername(String username) {
        this.username = username;
    }
    public String getPassword() {
        return password;
    }
    public void setPassword(String password) {
        this.password = password;
    }
    public String getEmail() {
        return email;
    }
    public void setEmail(String email) {
        this.email = email;
    }
    public int getPlayedGames() {
        return playedGames;
    }
    public void setPlayedGames(int playedGames) {
        this.playedGames = playedGames;
    }
    public int getLostGames() {
        return lostGames;
    }
    public void setLostGames(int lostGames) {
        this.lostGames = lostGames;
    }
    public int getWinGames() {
        return winGames;
    }
    public void setWinGames(int winGames) {
        this.winGames = winGames;
    }
}
