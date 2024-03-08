package it.unipi.dii.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;

public class CustomerDTO {
    private String name;
    private String surname;
    private String username;

    public CustomerDTO(String name, String surname, String username) {
        this.name = name;
        this.surname = surname;
        this.username = username;
    }

    public CustomerDTO() {}

    @JsonProperty("name")
    public String getName() {
        return this.name;
    }

    @JsonProperty("surname")
    public String getSurname() {
        return this.surname;
    }

    @JsonProperty("username")
    public String getUsername() {
        return this.username;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, surname, username);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CustomerDTO that = (CustomerDTO) o;
        return Objects.equals(name, that.name) &&
                Objects.equals(surname, that.surname) &&
                Objects.equals(username, that.username);
    }

    @Override
    public String toString() {
        return "CustomerDTO{" +
                "name='" + name + '\'' +
                ", surname='" + surname + '\'' +
                ", username='" + username + '\'' +
                '}';
    }
}
