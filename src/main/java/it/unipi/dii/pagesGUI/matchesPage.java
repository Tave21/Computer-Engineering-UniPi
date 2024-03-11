package it.unipi.dii.pagesGUI;

import com.mongodb.client.AggregateIterable;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.model.Match;
import it.unipi.dii.model.Multiplier;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.geometry.VPos;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.*;
import org.bson.Document;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.DateTimes.getCurrentDate;
import static it.unipi.dii.utility.JsonToDocument.convertDocumentToJson;
import static it.unipi.dii.utility.JsonToObjectConverter.convertJsonToObject;

public class matchesPage {
    private final int DAY_LOOK_UP = 10; // Shows every match with matchDate greater that today - DAY_LOOK_UP days.
    public StackPane getContent(boolean registered) {

        StackPane stackPane = new StackPane();

        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(50);

        VBox matchesContent = new VBox();
        matchesContent.setAlignment(Pos.CENTER);
        matchesContent.setSpacing(20);

        Label titleLabel = new Label("Football Matches");
        titleLabel.getStyleClass().add("live-title");

        matchesContent.getChildren().addAll(createColumns(registered), bottomSpacer);

        stackPane.getChildren().addAll(matchesContent);

        ScrollPane scrollPane = new ScrollPane(stackPane);
        scrollPane.setFitToWidth(true);
        scrollPane.getStyleClass().add("matches_scroll");

        VBox.setVgrow(scrollPane, Priority.ALWAYS);
        VBox box = new VBox();
        box.setAlignment(Pos.CENTER);
        VBox.setMargin(titleLabel, new Insets(20, 0, 20, 0));
        box.getChildren().addAll(topSpacer, titleLabel, scrollPane);
        return new StackPane(box);
    }
    private GridPane createColumns(boolean registered) {
        GridPane gridPane = new GridPane();
        gridPane.setAlignment(Pos.CENTER);
        gridPane.setHgap(20);

        String[] leagues = {"Premier League", "Serie A", "Bundesliga", "La Liga", "Ligue 1"};

        for (int i = 0; i < 5; i++) {
            ColumnConstraints column = new ColumnConstraints(500);

            Label columnTitle = new Label(leagues[i]);
            columnTitle = colorLeagueLabel(columnTitle);
            gridPane.getColumnConstraints().add(column);
            gridPane.add(columnTitle, i, 0);

            //add negative margin to create space below the title, otherwise the title overlaps
            GridPane.setMargin(columnTitle, new Insets(-5, 0, 0, 0));

            GridPane.setValignment(columnTitle, VPos.TOP);
            GridPane.setHalignment(columnTitle, HPos.CENTER);
        }

        MatchMongoDBDAO md = new MatchMongoDBDAO();
        md.openConnection();

        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("matchDate",
                                new Document("$gt", getCurrentDate().minusDays(DAY_LOOK_UP).toString()))),
                new Document("$sort",
                        new Document("matchDate", -1L)),
                new Document("$project",
                        new Document("_id", 0L)));

        AggregateIterable<Document> docs = md.mongoDB.getCollection("matches").aggregate(pipeline);

        // Set the row indexes.
        int it = 0;
        int es = 0;
        int ge = 0;
        int fr = 0;
        int gb = 0;

        for (Document document : docs) {
            Match m = convertJsonToObject(convertDocumentToJson(document), Match.class); // Convert the document to object.

            assert m != null;

            m.cleanGoals();
            // In some cases, the goals are null and JavaFX has problem with that.
            // So this function set them to 0 instead.

            if(Objects.equals(m.getCompetition_id(), "IT1")){
                gridPane.add(createColumn(registered, m.getMatchDate(), "Serie A", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 1, it);
                it++;
            }else if(Objects.equals(m.getCompetition_id(), "ES1")){
                gridPane.add(createColumn(registered, m.getMatchDate(), "La Liga", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 3, es);
                es++;
            }else if(Objects.equals(m.getCompetition_id(), "GB1")){
                gridPane.add(createColumn(registered, m.getMatchDate(), "Premier League", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 0, gb);
                gb++;
            }else if(Objects.equals(m.getCompetition_id(), "L1")){
                gridPane.add(createColumn(registered, m.getMatchDate(), "Bundesliga", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 2, ge);
                ge++;
            }else if(Objects.equals(m.getCompetition_id(), "FR1")){
                gridPane.add(createColumn(registered, m.getMatchDate(), "Ligue 1", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 4, fr);
                fr++;
            }
        }

        return gridPane;
    }
    // We create an instance of Live, and we use the
    // functions in common with the other class.
    private VBox createColumn(boolean registered, String matchday, String league, String team1, String team2, String score1, String score2, String minutes, List<Multiplier> multipliers) {
        livePage liveInstance = new livePage();
        return liveInstance.createColumn(registered, matchday, league, team1, team2, score1, score2, minutes, multipliers);
    }
    private Label colorLeagueLabel(Label label) {
        livePage liveInstance = new livePage();
        return liveInstance.colorLeagueLabel(label);
    }
}