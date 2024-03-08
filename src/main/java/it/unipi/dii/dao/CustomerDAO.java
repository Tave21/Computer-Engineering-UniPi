package it.unipi.dii.dao;

import it.unipi.dii.model.Customer;

import org.bson.Document;

import java.util.List;


public interface CustomerDAO {
    boolean registerCustomer(Customer user);
    Customer authenticateCustomer(String username, String password);
    Customer fetchCustomerInformation(String username);
    boolean customerInfoAlreadyPresent(Customer user);
    void removeCustomer(Document query);
    void removeCustomer(String username);
    void replaceCustomer(Document query, Customer newCustomer);
    void replaceCustomer(Customer newCustomer);
    List<Customer> getCustomers(Document query , Document projection);
    void redeem(String Username, double howMuch);
    boolean pay(String Username, double howMuch);
    void updateCustomer(Document query, Document update);
    double getCreditOfCustomer(String username);
}
