package it.unipi.dsmt.FleetFra.DAO;

import it.unipi.dsmt.FleetFra.DTO.*;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class UserDAO extends BaseDAO {

    //Method to hash the password
    private String hashPassword(String password) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(password.getBytes());
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                hexString.append(String.format("%02x", b));
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("Error", e);
        }
    }

    // DAO method for signing up a user: it returns an integer value to indicate the result of the operation
    public int signup(UserDTO user) {
        String checkExistingUserSQL = "SELECT * FROM FleetFra.user WHERE Username = ?";
        String registerUserSQL = "INSERT INTO FleetFra.user (Username, Name, Surname, Password, Email) VALUES (?,?,?,?,?);";

        try (Connection connection = getConnection()) {
            connection.setAutoCommit(false);

            // Check if the username already exists
            try (PreparedStatement checkStatement = connection.prepareStatement(checkExistingUserSQL)) {
                checkStatement.setString(1, user.getUsername());

                try (ResultSet resultSet = checkStatement.executeQuery()) {
                    if (resultSet.next()) {
                        // Username already exists, return an error
                        return 0;
                    }
                }
            } catch (SQLException ex) {
                ex.printStackTrace();
                return -1;
            }

            // Insert the new user with hashed password
            try (PreparedStatement preparedStatement = connection.prepareStatement(registerUserSQL)) {
                preparedStatement.setString(1, user.getUsername());
                preparedStatement.setString(2, user.getFirstName());
                preparedStatement.setString(3, user.getLastName());
                preparedStatement.setString(4, hashPassword(user.getPassword())); // Hash della password
                preparedStatement.setString(5, user.getEmail());

                if (preparedStatement.executeUpdate() == 0) {
                    connection.rollback();
                    return -1;
                }

                connection.commit();
            } catch (SQLException ex) {
                connection.rollback();
                ex.printStackTrace();
                return -1;
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            return -1;
        }

        return 1;
    }

    // DAO method for logging in a user: it returns a UserDTO object containing the user's information
    public UserDTO login(String username, String password) {
        String loginQuery = "SELECT * FROM FleetFra.user WHERE username = ? AND password = ?";

        try (Connection connection = getConnection(); PreparedStatement preparedStatement = connection.prepareStatement(loginQuery)) {
            preparedStatement.setString(1, username);
            preparedStatement.setString(2, hashPassword(password)); //compare the hashed password

            try (ResultSet resultSet = preparedStatement.executeQuery()) {
                if (resultSet.next()) {
                    UserDTO userDTO = new UserDTO();
                    userDTO.setUsername(resultSet.getString("Username"));
                    return userDTO;
                } else {
                    return null;
                }
            }

        } catch (SQLException ex) {
            ex.printStackTrace();
            return null;
        }
    }

    // DAO method for removing a user from the database
    public boolean removeUser(String username) {
        String removeQuery = "DELETE FROM FleetFra.user WHERE username = ?";

        try (Connection connection = getConnection(); PreparedStatement preparedStatement = connection.prepareStatement(removeQuery)) {
            preparedStatement.setString(1, username);
            return preparedStatement.executeUpdate() > 0;
        } catch (SQLException ex) {
            ex.printStackTrace();
            return false;
        }
    }

    // DAO method for viewing users: it returns a list of UserDTO objects
    public List<UserDTO> viewUsers(ViewUsersRequestDTO request, boolean includeStats) {
        List<UserDTO> entries = new ArrayList<>();
        String baseQuery = "SELECT Username, Surname, Name, Email FROM FleetFra.user";

        List<String> conditions = new ArrayList<>();
        List<Object> parameters = new ArrayList<>();

        if (request.getUsername() != null && !request.getUsername().isEmpty()) {
            conditions.add("Username LIKE ?");
            parameters.add(request.getUsername() + "%");
        }
        if (request.getFirstName() != null && !request.getFirstName().isEmpty()) {
            conditions.add("Name LIKE ?");
            parameters.add(request.getFirstName() + "%");
        }
        if (request.getLastName() != null && !request.getLastName().isEmpty()) {
            conditions.add("Surname LIKE ?");
            parameters.add(request.getLastName() + "%");
        }

        String finalQuery = baseQuery;
        if (!conditions.isEmpty()) {
            finalQuery += " WHERE " + String.join(" AND ", conditions);
        }

        try (Connection connection = getConnection(); PreparedStatement preparedStatement = connection.prepareStatement(finalQuery)) {
            for (int i = 0; i < parameters.size(); i++) {
                preparedStatement.setObject(i + 1, parameters.get(i));
            }

            ResultSet resultSet = preparedStatement.executeQuery();
            while (resultSet.next()) {
                UserDTO userDTO = new UserDTO();
                userDTO.setUsername(resultSet.getString("Username"));
                userDTO.setFirstName(resultSet.getString("Name"));
                userDTO.setLastName(resultSet.getString("Surname"));
                userDTO.setEmail(resultSet.getString("Email"));
                if (includeStats) {
                    calculateUserStats(connection, userDTO);
                }
                entries.add(userDTO);
            }
        } catch (SQLException e) {
            e.printStackTrace();
            return null;
        }
        return entries;
    }

    private void calculateUserStats(Connection conn, UserDTO user) {
        String matchQuery = "SELECT COUNT(*) AS playedGames, " +
                "SUM(CASE WHEN (User1 = ? AND Winner = 1) OR (User2 = ? AND Winner = 0) THEN 1 ELSE 0 END) AS winGames, " +
                "SUM(CASE WHEN (User1 = ? AND Winner = 0) OR (User2 = ? AND Winner = 1) THEN 1 ELSE 0 END) AS lostGames " +
                "FROM FleetFra.match WHERE User1 = ? OR User2 = ?";

        try (PreparedStatement stmt = conn.prepareStatement(matchQuery)) {
            for (int i = 1; i <= 6; i++) {
                stmt.setString(i, user.getUsername());
            }

            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                user.setPlayedGames(rs.getInt("playedGames"));
                user.setWinGames(rs.getInt("winGames"));
                user.setLostGames(rs.getInt("lostGames"));
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
}
