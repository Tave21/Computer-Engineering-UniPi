package it.unipi.dsmt.FleetFra.DTO;

import com.fasterxml.jackson.annotation.JsonInclude;

//null values (in class attributes) are not included in the json object
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserAccessRequest {
    private String username;
    private String password;

    public UserAccessRequest(String username, String password, Boolean isAdmin) {
        this.username = username;
        this.password = password;
    }
    public UserAccessRequest(){}

    public void setUsername(String username){
        this.username = username;
    }
    public String getUsername(){
        return this.username;
    }
    public void setPassword(String password){
        this.password = password;
    }
    public String getPassword(){
        return this.password;
    }


}
