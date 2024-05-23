package it.unipi.dii.model;
import com.fasterxml.jackson.annotation.JsonProperty;

public class User {
    protected String name;
    protected String surname;
    protected String email;
    protected String cellNumber;

    protected String password;

    public User() {
    }
    public User( String name, String surname, String email, String cellNumber, String password) {
        this.name = name;
        this.surname = surname;
        this.email = email;
        this.cellNumber = cellNumber;
        this.password = password;
    }
    @JsonProperty("name")
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
    @JsonProperty("surname")
    public String getSurname() {
        return surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }
    @JsonProperty("email")
    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }
    @JsonProperty("cellNumber")
    public String getCellNumber() {
        return cellNumber;
    }

    public void setCellNumber(String cellNumber) {
        this.cellNumber = cellNumber;
    }
    @JsonProperty("password")
    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public String toString() {
        return "Customer{" +
                ", name='" + name + '\'' +
                ", surname='" + surname + '\'' +
                ", password='" + password + '\'' +
                ", email ='" + email + '\'' +
                ", cellNumber =" + cellNumber+
                '}';
    }
}

