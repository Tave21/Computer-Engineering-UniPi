package it.unipi.dii.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;

public class Customer extends User {
    private String username;
    private String gender;
    private String birthDate;
    private String registrationDate;
    private String address;
    private String cityOfResidence;
    private String province;
    private double credit;

    public Customer() {
        this.credit = 0;
    }

    public Customer(
            String username, String gender, String birthDate, String registrationDate,
            String address, String cityOfResidence, String province, double credit) {
        this.username = username;
        this.gender = gender;
        this.birthDate = birthDate;
        this.registrationDate = registrationDate;
        this.address = address;
        this.cityOfResidence = cityOfResidence;
        this.province = province;
        this.credit = credit;
    }

    @JsonProperty("username")
    public String getUsername() {
        return this.username;
    }
    public void setUsername(String username) {
        this.username = username;
    }
    @JsonProperty("gender")
    public String getGender() {
        return this.gender;
    }
    public void setGender(String gender) {
        this.gender = gender;
    }
    @JsonProperty("birthDate")
    public String getBirthDate() {
        return this.birthDate;
    }

    public void setBirthDate(String birthDate) {
        this.birthDate = birthDate;
    }
    @JsonProperty("registrationDate")
    public String getRegistrationDate() {
        return this.registrationDate;
    }

    public void setRegistrationDate(String registrationDate) {
        this.registrationDate = registrationDate;
    }
    @JsonProperty("address")
    public String getAddress() {
        return this.address;
    }

    public void setAddress(String address) {
        this.address = address;
    }
    @JsonProperty("cityOfResidence")
    public String getCityOfResidence() {
        return this.cityOfResidence;
    }

    public void setCityOfResidence(String cityOfResidence) {
        this.cityOfResidence = cityOfResidence;
    }
    @JsonProperty("province")
    public String getProvince() {
        return this.province;
    }

    public void setProvince(String province) {
        this.province = province;
    }

    @JsonProperty("credit")
    public double getCredit() {
        return this.credit;
    }
    public void setCredit(double credit) {
        this.credit = credit;
    }

    /**
     * the customer pays the slip
     * @param howMuch is the bet amount inserted by the user
     * @return true if the customer has sufficient credit, false oterwise
     */
    public boolean pay(double howMuch){
        if(howMuch <= this.credit){
            this.credit = this.credit - howMuch;
            return true;
        }else{
            return false;
        }
    }
    /**
     * the customer add moneys to his/her credit
     * @param howMuch are the moneys inserted by the user
     */
    public void redeem(double howMuch){
        this.credit = this.credit + howMuch;
    }

    /**
     * create a string with all atributes of the Customer
     * @return a string in a JSON format
     */
    @Override
    public String toString() {
        return "Customer{" +
                " username = '" + this.username + '\'' +
                ", gender = '" + this.gender + '\'' +
                ", birthDate = '" + this.birthDate + '\'' +
                ", registrationDate = '" + this.registrationDate + '\'' +
                ", address = '" + this.address + '\'' +
                ", province = '" + this.province + '\'' +
                ", cityofResidence = '" + this.cityOfResidence + '\'' +
                ", credit = '" + this.credit + '\'' +
                '}';
    }
    /**
     * create a hash for the admin
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return Objects.hash(username, gender, birthDate, registrationDate, address, cityOfResidence, province, credit,
                name, surname, email, cellNumber, password);
    }

    /**
     * Check if a customer is equals to another one
     * @param obj is a customer
     * @return true if the two customers are equal, otherwise false
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;
        Customer user = (Customer) obj;
        return Double.compare(user.credit, credit) == 0 &&
                Objects.equals(username, user.username) &&
                Objects.equals(gender, user.gender) &&
                Objects.equals(birthDate, user.birthDate) &&
                Objects.equals(registrationDate, user.registrationDate) &&
                Objects.equals(address, user.address) &&
                Objects.equals(cityOfResidence, user.cityOfResidence) &&
                Objects.equals(province, user.province) &&
                Objects.equals(name, user.name) &&
                Objects.equals(surname, user.surname) &&
                Objects.equals(email, user.email) &&
                Objects.equals(cellNumber, user.cellNumber) &&
                Objects.equals(password, user.password);
    }
}

