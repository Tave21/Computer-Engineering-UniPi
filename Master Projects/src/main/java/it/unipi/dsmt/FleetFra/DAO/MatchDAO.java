package it.unipi.dsmt.FleetFra.DAO;

// browse matches
// insert match

import it.unipi.dsmt.FleetFra.DTO.*;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class MatchDAO extends  BaseDAO{

    // browse matches
    public List<MatchDTO> browseGames(String date1, String date2){
        //pageDTO contains the list of games to be displayed
        List<MatchDTO> list = new ArrayList<>();
        String browseMatchSQL = "SELECT * FROM FleetFra.match WHERE Timestamp BETWEEN ? AND ?";
        String date1Formatted = date1 + " 00:00:00";
        String date2Formatted = date2 + " 23:59:59";
        System.out.println(browseMatchSQL);

        // if username is null, the request is from an admin
        try (Connection connection = getConnection();
                 PreparedStatement preparedStatement = connection.prepareStatement(browseMatchSQL, PreparedStatement.RETURN_GENERATED_KEYS)) {
                 preparedStatement.setString(1, date1Formatted);
                 preparedStatement.setString(2, date2Formatted);
                 System.out.println("statement: " + preparedStatement);

                ResultSet resultSet = preparedStatement.executeQuery();
                while (resultSet.next()){
                    // create a new MatchDTO object and fill it with the data from the database
                    MatchDTO matchDTO = new MatchDTO();
                    matchDTO.setId(resultSet.getInt("idMatch"));
                    matchDTO.setUser1(resultSet.getString("User1"));
                    matchDTO.setUser2(resultSet.getString("User2"));
                    matchDTO.setTimestamp(resultSet.getString("Timestamp"));
                    if(Objects.equals(resultSet.getString("Winner"), "1"))
                        matchDTO.setWinner(matchDTO.getUser1());
                    else
                        matchDTO.setWinner(matchDTO.getUser2());
                    list.add(matchDTO);
                }
                // set the entries and the counter in the pageDTO object
            } catch (SQLException e){
                e.printStackTrace();
                return null;
            }


        return list;
    }

    // insert match
    public boolean insert(MatchDTO match) {
        String insertQuery = "INSERT INTO FleetFra.match" +
                "(User1, User2, Timestamp, Winner) " +
                "VALUES (?, ?, ?, ?)";
        //prepare the statement
        try (Connection connection = getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(insertQuery, PreparedStatement.RETURN_GENERATED_KEYS)) {
            //fill the statement with the data from the match object
            preparedStatement.setString(1, match.getUser1());
            preparedStatement.setString(2, match.getUser2());
            preparedStatement.setString(3, match.getTimestamp());
            preparedStatement.setString(4, match.getWinner());
            //run the query
            int rowsAffected = preparedStatement.executeUpdate();

            return rowsAffected > 0;
        } catch (SQLException ex) {
            ex.printStackTrace();
            return false;
        }
    }

}
