package it.unipi.dsmt.FleetFra.DAO;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class BaseDAO {
    // Database connection parameters
    private static final String URL = "jdbc:mysql://10.2.1.25:3306/FleetFra";
    private static final String USERNAME = "root";

    private static final String PASSWORD = "root";
    private static Connection connection = null;

    // Constructor
    public BaseDAO(){
        connection = getConnection();
    }

    // Close the connection to the db
    public void close() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
    // Get the connection to the db
    public Connection getConnection(){
        try{
            if (connection == null || connection.isClosed()){
                connection = DriverManager.getConnection(URL, USERNAME, PASSWORD);
            }
        }catch (SQLException e) {
            e.printStackTrace();
            throw new RuntimeException("Error during the connection to the db", e);
        }
        return connection;
    }
}
