package it.unipi.dsmt.FleetFra.DTO;

//manage the request for the view of the users
public class ViewUsersRequestDTO {
    private String firstName;
    private String lastName;
    private String username;

    public ViewUsersRequestDTO(){}

    public ViewUsersRequestDTO(String firstName, String lastName, String username ) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.username = username;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

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
}
