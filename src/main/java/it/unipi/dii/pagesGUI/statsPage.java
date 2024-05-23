package it.unipi.dii.pagesGUI;

import com.mongodb.client.MongoCursor;
import it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.financialResults.financialReport;
import it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.seventhQuery.mainReport;
import it.unipi.dii.dao.mongo.StatisticsMongoDBDAO;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Label;

import javafx.scene.control.ScrollPane;
import javafx.scene.layout.*;
import org.bson.Document;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import static it.unipi.dii.utility.converters.jsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.dateTimes.*;

public class statsPage {
    private final static int CHAMPIONSHIP_NUMBER = 5;

    public StackPane getContent() {
        StackPane stackPane = new StackPane();

        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(50);

        Label titleLabel = new Label("Statistics");
        titleLabel.getStyleClass().add("live-title");

        StatisticsMongoDBDAO st = new StatisticsMongoDBDAO();
        st.openConnection();

        List<String> financialArray = new ArrayList<>();

        try (MongoCursor<Document> cursor = st.mongoDB.getCollection("analytics")
                .find(new Document("type", "financial").append("periodRelated", getFirstDayOfMonth().toString()))
                .projection(new Document("_id", 0L))
                .iterator()) {
            financialReport f = convertJsonToObject(cursor.next().toJson(), financialReport.class);
            assert f != null;
            financialArray.add(String.valueOf(f.getValueList().get(0).getValue()));
        } catch (NullPointerException e) {
            financialArray.add("No info available!");
        }

        List<String> appreciatedTeams = st.showMostAppreciatedTeamsPolls();

        List<String> appreciatedPlayers = st.showMostAppreciatedPlayersPolls();

        mainReport m;
        List<String> topTeams = new ArrayList<>();
        try (MongoCursor<Document> cursor = st.mongoDB.getCollection("analytics")
                .find(new Document("type", "users favourite teams").append("periodRelated", getFirstDayOfMonth().toString()))
                .projection(new Document("_id", 0L))
                .iterator()) {
            m = convertJsonToObject(cursor.next().toJson(), mainReport.class);

            assert m != null;

            for(int i = 0 ; i < m.getValueList().size() ; i++){
                topTeams.add(m.getValueList().get(i).getChampionship_id());
            }

        } catch (NullPointerException e) {
            topTeams.clear();
        }

        List<String> averageMatches = new ArrayList<>();
        for (int i = 0; i < CHAMPIONSHIP_NUMBER; i++) {
            averageMatches.add("No info");
        }

        try (MongoCursor<Document> cursor = st.mongoDB.getCollection("analytics")
                .find(new Document("type", "seventh").append("periodRelated", getFirstDayOfMonth().toString()))
                .projection(new Document("_id", 0L))
                .iterator()) {
            m = convertJsonToObject(cursor.next().toJson(), mainReport.class);
            assert m != null;

            for (int i = 0; i < m.getValueList().size() ; i++) {
                String id = m.getValueList().get(i).getChampionship_id();
                String howMany = String.valueOf(m.getValueList().get(i).getValue());

                if (Objects.equals(id, "GB1")) {
                    averageMatches.set(0, howMany);
                } else if (Objects.equals(id, "IT1")) {
                    averageMatches.set(1, howMany);
                } else if (Objects.equals(id, "L1")) {
                    averageMatches.set(2, howMany);
                } else if (Objects.equals(id, "ES1")) {
                    averageMatches.set(3, howMany);
                } else if (Objects.equals(id, "FR1")) {
                    averageMatches.set(4, howMany);
                }
            }

        } catch (NullPointerException e) {
            for (int i = 0; i < CHAMPIONSHIP_NUMBER; i++) {
                averageMatches.add("No info");
            }
        }

        st.closeConnection();

        VBox financialResults = createStatElement("Financial results", financialArray);
        financialResults.setMaxWidth(300);
        VBox mostAppreciatedTeams = createStatElement("Most appreciated teams", appreciatedTeams);
        mostAppreciatedTeams.setMaxWidth(300);
        VBox mostAppreciatedPlayers = createStatElement("Most appreciated  players", appreciatedPlayers);
        mostAppreciatedPlayers.setMaxWidth(300);
        VBox averageMatchesForSlip = createStatElement("Average number of matches per championship", averageMatches);
        averageMatchesForSlip.setMaxWidth(300);

        VBox topThreeTeams = createStatElement("Top bet teams by users", topTeams);
        topThreeTeams.setMaxWidth(300);

        VBox content = new VBox();
        content.setMaxWidth(800);
        content.setAlignment(Pos.CENTER);
        content.setSpacing(20);
        content.getChildren().addAll(topSpacer, titleLabel, financialResults, mostAppreciatedTeams, mostAppreciatedPlayers, averageMatchesForSlip, topThreeTeams, bottomSpacer);

        stackPane.getChildren().addAll(content);
        ScrollPane scrollPane = new ScrollPane(stackPane);
        scrollPane.setFitToWidth(true);
        scrollPane.getStyleClass().add("matches_scroll");

        VBox.setVgrow(scrollPane, Priority.ALWAYS);

        return new StackPane(scrollPane);
    }

    protected VBox createStatElement(String title, List<String> array) {
        String[] championships = {"Premier League", "Serie A", "Bundesliga", "La Liga", "Ligue 1"};
        VBox form = new VBox();
        form.getStyleClass().addAll("form1", "form-container1");
        VBox.setVgrow(form, Priority.ALWAYS);

        form.setAlignment(Pos.CENTER);
        form.setSpacing(15);
        form.setPadding(new Insets(10, 10, 10, 10));

        Label titleLabel = new Label(title);
        titleLabel.getStyleClass().add("input-label");

        switch (title) {
            case "Most appreciated teams":
            case "Most appreciated  players":
            case "Top bet teams by users":
                form.getChildren().add(titleLabel);
                for (int i = 0; i < array.size(); i++) {
                    HBox regionBox = createTeamBox(array.get(i), i);
                    form.getChildren().add(regionBox);
                }
                break;
            case "Average users' age":
                form.getChildren().add(titleLabel);
                HBox regionBox = createRegionBox(array.get(0));
                form.getChildren().add(regionBox);
                break;
            case "Financial results":
                form.getChildren().add(titleLabel);
                for (int i = 0; i < array.size(); i++) {
                    HBox regionBox1 = createFinancialBox(array.get(0));
                    form.getChildren().add(regionBox1);
                }
                break;
            case "Average number of matches per championship":
                form.getChildren().add(titleLabel);
                for (int i = 0; i < array.size(); i++) {
                    HBox regionBox2 = createMatchesBox(championships[i], array.get(i));
                    form.getChildren().add(regionBox2);
                }
                break;
        }

        return form;
    }

    private HBox createRegionBox(String averageAge) {
        Label regionLabel = new Label("Average age: ");
        regionLabel.getStyleClass().add("input-label");
        Label ageLabel = new Label(averageAge);
        ageLabel.getStyleClass().add("user-label");

        HBox regionBox = new HBox(10);
        regionBox.getChildren().addAll(regionLabel, ageLabel);

        return regionBox;
    }

    private HBox createTeamBox(String team, int i) {
        Label teamLabel = new Label((i + 1) + ") ");
        teamLabel.getStyleClass().add("input-label");
        Label valueLabel = new Label(team);
        valueLabel.getStyleClass().add("user-label");

        HBox regionBox = new HBox(10);
        regionBox.getChildren().addAll(teamLabel, valueLabel);

        return regionBox;
    }

    private HBox createFinancialBox(String value) {
        Label financialLabel = new Label("");

        financialLabel.setText("Total revenue: ");
        financialLabel.getStyleClass().add("input-label");

        Label valueLabel = new Label(value);
        valueLabel.getStyleClass().add("user-label");
        Label moneyLabel = new Label("€");
        moneyLabel.getStyleClass().add("user-label");

        HBox regionBox = new HBox(10);
        regionBox.getChildren().addAll(financialLabel, valueLabel, moneyLabel);

        return regionBox;
    }

    private HBox createMatchesBox(String league, String value) {
        Label leagueLabel = new Label(league);
        leagueLabel = colorLeagueLabel(leagueLabel);
        leagueLabel.setText(leagueLabel.getText() + ": ");

        double num;
        if(!Objects.equals(value, "No info")) {
            num = Double.parseDouble(value);
        }else{
            num = 0;
        }
        value = String.format("%.4f", num);

        Label valueLabel = new Label(value);
        valueLabel.getStyleClass().add("user-label");

        HBox regionBox = new HBox(10);
        regionBox.getChildren().addAll(leagueLabel, valueLabel);

        return regionBox;
    }

    private Label colorLeagueLabel(Label label) {
        livePage liveInstance = new livePage(); // Create an instance of Live.
        return liveInstance.colorLeagueLabel(label);
    }
}