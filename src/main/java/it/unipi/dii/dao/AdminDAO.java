package it.unipi.dii.dao;

import it.unipi.dii.model.Admin;

import org.bson.Document;

import java.util.List;

public interface AdminDAO {
    Admin authenticateAdmin(String email, String password);
    Admin fetchAdminInformation(String email);
    List<Admin> getAdmins(Document query , Document projection);
}
