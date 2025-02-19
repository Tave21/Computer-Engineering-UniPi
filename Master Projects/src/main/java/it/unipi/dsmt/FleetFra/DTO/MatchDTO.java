package it.unipi.dsmt.FleetFra.DTO;


public class MatchDTO {
    //attributes of the MatchDTO object
    private int id;
    private String user1;
    private String user2;
    private String timestamp;
    private String winner;


    public MatchDTO(){}

    public MatchDTO(int id, String user1, String user2, String timestamp, String winner) {
        this.id = id;
        this.user1 = user1;
        this.user2 = user2;
        this.timestamp = timestamp;
        this.winner= winner;
    }

    public MatchDTO(String user1, String user2, String timestamp, String winner) {
        this.user1 = user1;
        this.user2 = user2;
        this.timestamp = timestamp;
        this.winner= winner;

    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getUser1() {
        return user1;
    }

    public void setUser1(String user1) {
        this.user1 = user1;
    }

    public String getUser2() {
        return user2;
    }

    public void setUser2(String user2) {
        this.user2 = user2;
    }


    public String getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(String timestamp) {
        this.timestamp = timestamp;
    }

    public String getWinner() {
        return winner;
    }

    public void setWinner(String winner) {
        this.winner= winner;
    }
}
