package it.unipi.dii.pagesGUI;

// Classe rappresentante un utente
public class userPage {
    private String username;
    private String name;
    private String surname;

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
