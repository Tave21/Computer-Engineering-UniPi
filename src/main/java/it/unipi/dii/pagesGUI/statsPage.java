package it.unipi.dii.pagesGUI;

import com.mongodb.client.AggregateIterable;
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

import static it.unipi.dii.utility.DateTimes.getCurrentDate;
import static it.unipi.dii.utility.DateTimes.getCurrentDateString;


public class statsPage {

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
        financialArray.add(String.valueOf(st.showFinancialResults(getCurrentDate().minusYears(2).toString(), getCurrentDateString())));

        List<String> appreciatedTeams = st.showMostAppreciatedTeamsPolls();

        List<String> appreciatedPlayers = st.showMostAppreciatedPlayersPolls();

        AggregateIterable<Document> av = st.averageNumberOfMatchesForEachChampionshipForEachSlip(getCurrentDate().minusYears(1).toString(), getCurrentDateString());

        List<String> topTeams = st.showUsersFavouriteTeams(getCurrentDate().minusYears(1).toString(), getCurrentDateString(), 10);

        List<String> averageMatches = new ArrayList<>();

        for (int i = 0; i < 5; i++) {
            averageMatches.add("No Info");
        }

        for (Document document : av) {
            String id = document.getString("_id");
            String howMany = String.valueOf(document.getDouble("HowMany"));

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

        st.closeConnection();

        VBox financialResults = createStatElement("Financial results", financialArray);
        financialResults.setMaxWidth(300);
        VBox mostAppreciatedTeams = createStatElement("Most appreciated teams", appreciatedTeams);
        mostAppreciatedTeams.setMaxWidth(300);
        VBox mostAppreciatedPlayers = createStatElement("Most appreciated  players", appreciatedPlayers);
        mostAppreciatedPlayers.setMaxWidth(300);
        VBox averageMatchesForSlip = createStatElement("Average number of matches per championship", averageMatches);
        averageMatchesForSlip.setMaxWidth(300);

        System.out.println(topTeams.toString());
        VBox topThreeTeams = createStatElement("Top betted teams by users", topTeams);
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
            case "Top betted teams by users":
                //they use the same code
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

        double num = Double.parseDouble(value);
        value = String.format("%.4f", num);

        Label valueLabel = new Label(value);
        valueLabel.getStyleClass().add("user-label");

        HBox regionBox = new HBox(10);
        regionBox.getChildren().addAll(leagueLabel, valueLabel);

        return regionBox;
    }
    private Label colorLeagueLabel(Label label) {
        livePage liveInstance = new livePage(); //create instance of Live
        return liveInstance.colorLeagueLabel(label);
    }
}