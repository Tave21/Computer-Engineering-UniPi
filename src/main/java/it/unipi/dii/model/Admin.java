package it.unipi.dii.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;

public class Admin extends User {
    String hiredDate;
    String title;
    public Admin() {
    }
    public Admin(String title, String hiredDate) {
        this.title = title;
        this.hiredDate = hiredDate;
    }

    @JsonProperty("title")
    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }
    @JsonProperty("hiredDate")
    public String getHiredDate() {
        return hiredDate;
    }

    public void setHiredDate(String hiredDate) {
        this.hiredDate = hiredDate;
    }

    @Override
    public String toString() {
        return "Admin{" +
                "name='" + this.name + '\'' +
                ", surname='" + this.surname + '\'' +
                ", email='" + this.email + '\'' +
                ", cellNumber='" + this.cellNumber + '\'' +
                ", password='" + this.password + '\'' +
                ", hiredDate='" + this.hiredDate + '\'' +
                ", title='" + this.title + '\'' +
                '}';
    }


    @Override
    public int hashCode() {
        return Objects.hash(name, surname, email, cellNumber, password, hiredDate, title);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;
        Admin admin = (Admin) obj;
        return Objects.equals(name, admin.name) &&
                Objects.equals(surname, admin.surname) &&
                Objects.equals(email, admin.email) &&
                Objects.equals(cellNumber, admin.cellNumber) &&
                Objects.equals(password, admin.password) &&
                Objects.equals(hiredDate, admin.hiredDate) &&
                Objects.equals(title, admin.title);
    }
}