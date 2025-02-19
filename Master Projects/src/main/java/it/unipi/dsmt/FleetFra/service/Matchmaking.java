package it.unipi.dsmt.FleetFra.service;

// Matchmaking class: stores the player1, player2 and matchId of a match to manage the matchmaking
public class Matchmaking {
    String player1;
    String player2;
    String matchId;

    public Matchmaking(String player1, String player2, String matchId) {
        this.player1 = player1;
        this.player2 = player2;
        this.matchId = matchId;
    }
    public String getPlayer1() { return player1; }
    public String getPlayer2() { return player2; }
    public String getMatchId() { return matchId; }
    public void setPlayer1(String player1) { this.player1 = player1; }
    public void setPlayer2(String player2) { this.player2 = player2; }
    public void setMatchId(String matchId) { this.matchId = matchId; }
}
