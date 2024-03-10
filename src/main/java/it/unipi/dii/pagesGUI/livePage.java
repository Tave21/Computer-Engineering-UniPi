package it.unipi.dii.pagesGUI;

import com.mongodb.client.AggregateIterable;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.model.Match;
import it.unipi.dii.model.Multiplier;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.geometry.VPos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.DateTimes.differenceDays;
import static it.unipi.dii.utility.DateTimes.getCurrentDateString;
import static it.unipi.dii.utility.JsonToDocument.convertDocumentToJson;
import static it.unipi.dii.utility.JsonToObjectConverter.convertJsonToObject;

public class livePage {

    private Stage dialog = new Stage();

    public Stage getDialog() {
        return dialog;
    }
    public StackPane getContent(boolean registered) {
        //main container of Live Football Results page
        StackPane stackPane = new StackPane();

        //add white space as a node before the title
        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20); // Desired height

        //add an empty space as a node
        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(50);

        //add title
        Label titleLabel = new Label("Live Football Results");
        titleLabel.getStyleClass().add("live-title");

        //add everything to the content
        VBox liveContent = new VBox();
        liveContent.setMaxWidth(300);
        liveContent.setAlignment(Pos.CENTER);
        liveContent.setSpacing(20);
        liveContent.getChildren().addAll(createColumns(registered), bottomSpacer);

        //add page content to a StackPane
        stackPane.getChildren().addAll(liveContent);
        //add the StackPane to a ScrollPane
        ScrollPane scrollPane = new ScrollPane(stackPane);
        scrollPane.setFitToWidth(true);
        scrollPane.getStyleClass().add("matches_scroll");

        //set center alignment for ScrollPane
        VBox.setVgrow(scrollPane, Priority.ALWAYS);
        VBox box = new VBox();
        box.setAlignment(Pos.CENTER);
        box.setMargin(titleLabel, new Insets(20, 0, 20, 0));
        box.getChildren().addAll(topSpacer, titleLabel, scrollPane);

        return new StackPane(box);
    }

    private GridPane createColumns(boolean registered) {
        GridPane gridPane = new GridPane();
        gridPane.setAlignment(Pos.CENTER);
        gridPane.setHgap(20); //Horizontal gap between columns.

        String[] leagues = {"Premier League", "Serie A", "Bundesliga", "La Liga", "Ligue 1"};

        for (int i = 0; i < 5; i++) {
            ColumnConstraints column = new ColumnConstraints(300); //Desired fixed width for each column

            Label columnTitle = new Label(leagues[i]);
            columnTitle = colorLeagueLabel(columnTitle);

            gridPane.getColumnConstraints().add(column);
            gridPane.add(columnTitle, i, 0);
            //add margin to create space below the title, negative, otherwise title overlaps
            gridPane.setMargin(columnTitle, new Insets(-5, 0, 0, 0));

            gridPane.setValignment(columnTitle, VPos.TOP);
            gridPane.setHalignment(columnTitle, HPos.CENTER); //set horizontal alignment to center
        }

        MatchMongoDBDAO md = new MatchMongoDBDAO();
        md.openConnection();

        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("status",
                                new Document("$in", Arrays.asList("IN_PLAY", "PAUSED")))),
                new Document("$sort",
                        new Document("matchDate", -1L)),
                new Document("$project",
                        new Document("_id", 0L)));

        AggregateIterable<Document> docs = md.mongoDB.getCollection("matches").aggregate(pipeline);

        int it = 0;
        int es = 0;
        int ge = 0;
        int fr = 0;
        int gb = 0;

        for (Document document : docs) {
            Match m = convertJsonToObject(convertDocumentToJson(document), Match.class);

            assert m != null;
            if(m.getStatus().equals("TIMED") && differenceDays(m.getMatchDate(), getCurrentDateString()) <= 0) {
                if (Objects.equals(m.getCompetition_id(), "IT1")) {
                    gridPane.add(createColumn(registered, m.getMatchDate(), "Serie A", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 1, it);
                    it++;
                } else if (Objects.equals(m.getCompetition_id(), "ES1")) {
                    gridPane.add(createColumn(registered, (m.getMatchDate()), "La Liga", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 3, es);
                    es++;
                } else if (Objects.equals(m.getCompetition_id(), "GB1")) {
                    gridPane.add(createColumn(registered, (m.getMatchDate()), "Premier League", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 0, gb);
                    gb++;
                } else if (Objects.equals(m.getCompetition_id(), "L1")) {
                    gridPane.add(createColumn(registered, (m.getMatchDate()), "Bundesliga", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 2, ge);
                    ge++;
                } else if (Objects.equals(m.getCompetition_id(), "FR1")) {
                    gridPane.add(createColumn(registered, (m.getMatchDate()), "Ligue 1", m.getTeam_home(), m.getTeam_away(), m.getHome_goals().toString(), m.getAway_goals().toString(), m.getStatus(), m.getMultipliers()), 4, fr);
                    fr++;
                }
            }
        }


        return gridPane;
    }

    public VBox createColumn(boolean registered, String matchday, String league, String team1, String team2, String score1, String score2, String minutes,  List<Multiplier> Multipliers) {
        VBox column = new VBox();
        column.setAlignment(Pos.CENTER);
        column.setSpacing(20);

        //void column to have space between the forms
        Label titleLabel = new Label("");

        //add matches results
        VBox contentBox = new VBox();
        contentBox.getChildren().addAll(
                titleLabel,
                createMatchResult(registered, matchday, league, team1, team2, score1, score2, minutes, Multipliers));
        column.getChildren().add(contentBox);

        return column;
    }

    public VBox createMatchResult(boolean registered, String matchday, String league, String team1, String team2, String score1, String score2, String minutes, List<Multiplier> Multipliers) {
        //single row for the match result
        VBox form = new VBox();
        form.getStyleClass().addAll("form", "form-container-slips");

        form.setAlignment(Pos.CENTER);
        form.setSpacing(5);

        Label leagueLabel = new Label(league);
        //we assign the color based on the league
        leagueLabel=colorLeagueLabel(leagueLabel);

        //check if the content is the live page or the matches one, in the first case, length is 3, but
        //in case we want to add the extratime, it will be 5
        String correctDate = matchday.replace("T", " ");
        correctDate = correctDate.replace("Z", "");
        correctDate =  correctDate.substring(0,  correctDate.length() - 3);
        if(minutes.length() > 5){

            Label matchdayLabel = new Label(correctDate);
            //the matchday label has the style of the relative league
            matchdayLabel.getStyleClass().addAll(leagueLabel.getStyleClass());
            leagueLabel.setText(matchdayLabel.getText());

        }else{
            leagueLabel.setText(leagueLabel.getText() +" - "+correctDate);
        }

        Label minutesLabel = new Label(minutes);
        minutesLabel.getStyleClass().addAll("input-label");

        Label team1Label = new Label(team1);
        team1Label.getStyleClass().addAll("input-label");
        Label score1Label = new Label(score1);
        score1Label.getStyleClass().addAll("input-label");


        Label team2Label = new Label(team2);
        team2Label.getStyleClass().addAll("input-label");
        Label score2Label = new Label(score2);
        score2Label.getStyleClass().addAll("input-label");


        //if the match has already been played, we don't add buttons, otherwise, we add buttons
        if(minutes.contains("FINISHED") || !registered){

            if(minutes.contains("TIMED")) {
                form.getStyleClass().addAll("form2", "user-container");
                minutesLabel.getStyleClass().addAll("user-label");
                team1Label.getStyleClass().addAll("user-label");
                score1Label.getStyleClass().addAll("user-label");
                team2Label.getStyleClass().addAll("user-label");
                score2Label.getStyleClass().addAll("user-label");
            }

            HBox row1 = createRow(leagueLabel, minutesLabel, false);
            HBox row2 = createRow(team1Label, score1Label, false);
            HBox row3 = createRow(team2Label, score2Label, true);
            form.getChildren().addAll(row1, row2, row3);
        }else{
            boolean in_play = false;
            if(!minutes.contains("IN_PLAY")) {
                form.getStyleClass().addAll("form2", "form-container");
                minutesLabel.getStyleClass().addAll("user-label");
                team1Label.getStyleClass().addAll("user-label");
                score1Label.getStyleClass().addAll("user-label");
                team2Label.getStyleClass().addAll("user-label");
                score2Label.getStyleClass().addAll("user-label");

            }else{
               in_play=true;
            }
            HBox row1 = createRow(leagueLabel, minutesLabel, false);
            HBox row2 = createRow(team1Label, score1Label, false);
            HBox row3 = createRow(team2Label, score2Label, true);

            HBox buttonsRow = createButtonsRow(Multipliers, team1, team2, matchday, in_play);

            form.getChildren().addAll(row1, row2, row3, buttonsRow);
            //region in order to have a blank space under the buttons
            Region bottomSpacer = new Region();
            VBox.setVgrow(bottomSpacer, Priority.ALWAYS);
            form.getChildren().add(bottomSpacer);
        }


        return form;
    }

    protected HBox createButtonsRow(List<Multiplier> Multipliers,String team1, String team2, String matchday, boolean in_play) {

        String team_concat = team1 + "_" + team2 + "_" + matchday;// milan inter

        List<Button> butList = new ArrayList<>();
        for (Multiplier multiplier : Multipliers){
            String correct_multiplier = String.format("%.1f", multiplier.getValue());
            butList.add(createBetButton(
                    multiplier.getName() + "   " + correct_multiplier
                    , team_concat, in_play));
        }

        List<VBox> VList = new ArrayList<>();

        for (int i = 0 ; i < butList.size() ; i++) {
            VBox buttonsRow;
            if(butList.size() - i == 1){
                 buttonsRow = new VBox(
                        5,
                        butList.get(i),
                        butList.get(i+1),
                        butList.get(i+2),
                        butList.get(i+3)
                );
            }else if(butList.size() - i == 2){
                buttonsRow = new VBox(
                        5,
                        butList.get(i),
                        butList.get(i+1),
                        butList.get(i+2)
                );
            }else if(butList.size() - i == 3){
                buttonsRow = new VBox(
                        5,
                        butList.get(i),
                        butList.get(i+1)
                );
            }else if(butList.size() - i == 4){
                buttonsRow = new VBox(
                        5,
                        butList.get(i)
                );
            }else if(butList.size() - i >= 5){
                buttonsRow = new VBox(
                        5,
                        butList.get(i),
                        butList.get(i+1),
                        butList.get(i+2),
                        butList.get(i+3),
                        butList.get(i+4)
                );
            }else{
                break;
            }

            VList.add(buttonsRow);
            i = i + 5;
        }

        for (VBox vBox : VList) {
            vBox.setAlignment(Pos.CENTER);
        }


        HBox buttonsRow = new HBox(10,
                VList.get(0),
                VList.get(1),
                VList.get(2),
                VList.get(3)
        );

        buttonsRow.setAlignment(Pos.CENTER);

        return buttonsRow;
    }

    protected Button createBetButton(String label, String teams, boolean in_play) {
        Button button = new Button(label);
        button.getStyleClass().add("multipliers-buttons");

        if(in_play){
            button.setDisable(true);
        }

        dialog.initModality(Modality.APPLICATION_MODAL);//set modal mode to block interaction with the main window

        button.setPrefWidth(100); //button width
        button.setPrefHeight(35); //button height

        button.setOnAction(event -> {

            String[] values = label.split("   "); //mettere label al posto di val
            String un = values[0];
            String du = values[1];
            String concat = un + "_" + du + "_" + teams; //1X_2.1_Milan_Inter_2022-01-01

            dialog.close();

            if (!dialog.isShowing()) {
                //create a new dialog window
                dialog.setTitle("Choose Slip"); //dialog window title

                //view the slips
                addToSlipPage pageToShow = new addToSlipPage(this);
                StackPane pageContent = pageToShow.getContent(concat);
                //new layout for the dialog window and add the content
                BorderPane dialogLayout = new BorderPane();
                dialogLayout.setCenter(pageContent);

                //dialog window dimensions
                dialog.setWidth(600);
                dialog.setHeight(400);

                Scene dialogScene = new Scene(dialogLayout);
                dialogScene.getStylesheets().add(getClass().getResource("/window_style.css").toExternalForm());
                dialog.setScene(dialogScene);

                //show dialog window
                if (!dialog.isShowing()) {
                    dialog.show();
                }
            }
        });

        return button;
    }
    protected HBox createRow(Label label1, Label label2, boolean lastRow){

        Region spacingRegion = new Region();
        HBox.setHgrow(spacingRegion, Priority.ALWAYS);
        HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 55));
        HBox row = new HBox(label1,  spacingRegion, label2);

        //if it is the last row in the HBox, we insert a 5 below margin
        if(lastRow) {
            // Imposta margini
            HBox.setMargin(label1, new Insets(5, 5, 5, 5));
            HBox.setMargin(label2, new Insets(5, 5, 5, 5));
        }else{
            HBox.setMargin(label1, new Insets(5, 5, 0, 5));
            HBox.setMargin(label2, new Insets(5, 5, 0, 5));
        }
        return row;
    }
    public Label colorLeagueLabel(Label label){
        if(label.getText().equals("Premier League")){
            label.getStyleClass().addAll("premier-label");
        }else if(label.getText().equals("Serie A")){
            label.getStyleClass().addAll("serie-label");
        }else if(label.getText().equals("Bundesliga")){
            label.getStyleClass().addAll("bundes-label");
        }else if(label.getText().equals("La Liga")){
            label.getStyleClass().addAll("liga-label");
        }else if(label.getText().equals("Ligue 1")){
            label.getStyleClass().addAll("ligue-label");
        }
        return label;
    }
}