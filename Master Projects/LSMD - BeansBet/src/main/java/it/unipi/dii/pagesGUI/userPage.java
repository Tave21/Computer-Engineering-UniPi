package it.unipi.dii.pagesGUI;

public class userPage {
    private final String username;
    private final String name;
    private final String surname;

    public userPage(String username, String name, String surname) {
        this.username = username;
        this.name = name;
        this.surname = surname;
    }

    public String getUsername() {
        return username;
    }
    public String getName() {
        return name;
    }

    public String getSurname() {
        return surname;
    }

    public String toString() {
        return "Username: " + username + ", Name: " + name + ", Surname: " + surname;
    }
}
