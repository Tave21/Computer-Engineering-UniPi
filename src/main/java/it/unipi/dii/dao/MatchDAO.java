package it.unipi.dii.dao;
import it.unipi.dii.model.Match;
import org.bson.Document;

import java.util.List;

public interface MatchDAO {
    void addMatch(Match m);
    void removeMatch(Document query);
    Integer getLastID();
    Integer getID(Match m);
    List<Match> getMatches(Document query, Document projection);
    Match getMatch(Integer matchID);
}
