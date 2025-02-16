package it.unipi.dii.pagesGUI;

import it.unipi.dii.dao.redis.SlipRedisDAO;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Slip;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.dateTimes.getCurrentInstantString;

public class addToSlipPage {
    private final livePage live;
    public addToSlipPage(livePage live) {
        this.live = live;
    }

    public  StackPane getContent(String concat) {
        StackPane stackPane = new StackPane();

        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(50);

        Label titleLabel = new Label("Choose Slip");
        titleLabel.getStyleClass().add("live-title");

        VBox slipsCartColumn = createSlipsColumn(concat);

        HBox slipsContent = new HBox();
        slipsContent.setAlignment(Pos.CENTER);
        slipsContent.setSpacing(20);
        slipsContent.getChildren().addAll(slipsCartColumn);

        VBox addToSlipContent = new VBox();
        addToSlipContent.setMaxWidth(1300);
        addToSlipContent.setAlignment(Pos.CENTER);
        addToSlipContent.setSpacing(20);
        addToSlipContent.getChildren().addAll(topSpacer, titleLabel, slipsContent, bottomSpacer);

        stackPane.getChildren().addAll(addToSlipContent);

        ScrollPane scrollPane = new ScrollPane(stackPane);
        scrollPane.setFitToWidth(true);
        scrollPane.getStyleClass().add("matches_scroll");

        VBox.setVgrow(scrollPane, Priority.ALWAYS);

        return new StackPane(scrollPane);
    }

    protected VBox createNewSlip(String concat) {
        //creation of a column for the slips
        VBox newSlip = new VBox();
        newSlip.getStyleClass().addAll("form", "form-container");
        newSlip.setAlignment(Pos.CENTER);
        newSlip.setSpacing(10);

        HBox firstAdd = new HBox();
        Label textLabel = new Label("Add bet to a new slip");
        textLabel.getStyleClass().add("input-label");
        Button addButton = new Button("Add");
        addButton.getStyleClass().add("right-buttons");

        Region spacingRegion = new Region();
        HBox.setHgrow(spacingRegion, Priority.ALWAYS);  // The second label to the right.
        HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 55));

        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(2);
        firstAdd.getChildren().addAll(textLabel, spacingRegion, addButton, bottomSpacer);

        VBox.setMargin(firstAdd, new Insets(5, 5, 5, 5));

        VBox.setMargin(newSlip, new Insets(0, 0, 10, 0)); //margin, to distance all slips

        //add everything to the column
        newSlip.getChildren().addAll(firstAdd);

        //add an action to the button to close the current window
        addButton.setOnAction(event -> {
            if (live.getDialog() != null && live.getDialog().isShowing()) {

                Slip slip = new Slip();
                slip.setCreationDate(getCurrentInstantString());
                slip.setUsername(Session.getUsername());
                slip.setBetAmount(2);
                String[] values = concat.split("_");

                //access to single values
                String value1 = values[0]; // "1X"
                String value2 = values[1]; // "2.1"
                String value3 = values[2]; // "Milan"
                String value4 = values[3]; // "Inter"
                String value5 = values[4]; // "2021-01-01"
                List<Bet> betlist = new ArrayList<>();
                Bet bet = new Bet();
                bet.setChosenMultiplierName(value1);
                //multiplier values have the comma, but a double needs the dot, so we change the comma in dot
                String changeComma = value2.replace(',', '.');
                bet.setChosenMultiplierValue(Double.parseDouble(changeComma));
                bet.setTeamHome(value3);
                bet.setTeamAway(value4);
                bet.setMatchDate(value5);
                bet.setCompetition_id("null");
                bet.setMatchID(0); //added while confirmation
                bet.setWin(-1);
                betlist.add(bet);
                slip.setBetsList(betlist);
                SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
                List<Slip> list;
                try {
                    list = slipRedisDAO.getListFromUser(Session.getUsername());
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
                int i = 1; //take the first index available
                for (Slip s : list) {
                    if (i != s.getSlipID()) {
                        break;
                    }
                    i++;
                }
                slip.setSlipID(i);
                int x = slipRedisDAO.create_Slip(slip);
                //match that you are trying to add is already started
                if (x == 1) {
                    //message
                    Label errorLabel = new Label("This match is now in play");
                    errorLabel.getStyleClass().add("error");
                    newSlip.getChildren().add(errorLabel);
                    addButton.setDisable(true);
                }else{
                    live.getDialog().close();
                }
            }

        });

        return newSlip;
    }
    protected VBox createSlipsColumn(String concat) {
        //creation of a column for the slips
        VBox column = new VBox();
        column.setAlignment(Pos.TOP_CENTER);

        VBox firstLine;
        firstLine = createNewSlip(concat);
        column.getChildren().add(firstLine);

        SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
        List<Slip> list;
        try {
            list = slipRedisDAO.getListFromUser(Session.getUsername());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        String[] values = concat.split("_");
        String teams = values[2] + "-" + values[3];
        for (Slip s : list){
            String[] matchArray = new String[s.findBetsList().size()];
            String[] multiplierArray = new String[s.findBetsList().size()];
            int i = 0;
            boolean f = true;
            for (Bet b : s.findBetsList()){
                matchArray[i] = b.getTeamHome() + "-" + b.getTeamAway();
                if(Objects.equals(matchArray[i], teams)){
                   f = false;
               }
                multiplierArray[i] = b.getChosenMultiplierName() + "   " + b.getChosenMultiplierValue();
                i++;
            }
            VBox slip = createSlip(matchArray, multiplierArray,s.getSlipID(),concat,f);
            VBox.setMargin( slip, new Insets(0, 0, 10, 0)); //margin, to distance all slips
            column.getChildren().add(slip);
        }


        return column;
    }
    protected VBox createSlip(String[] match, String[] multiplier, Integer id, String concat,boolean f) {
        //creation of a single slip
        VBox slip = new VBox();
        slip.getStyleClass().addAll("form", "form-container");
        slip.setAlignment(Pos.CENTER);
        slip.setSpacing(10);
        for (int i = 0; i < match.length; i++) {
            //add match label
            Label matchLabel = new Label(match[i]);
            matchLabel.getStyleClass().add("input-label");

            //add multiplier label
            Label multiplierLabel = new Label(multiplier[i]);
            multiplierLabel.getStyleClass().add("input-label");

            Region spacingRegion = new Region();
            HBox.setHgrow(spacingRegion, Priority.ALWAYS);  // Second label to the right.
            HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 55));

            //create a horizontal line for each match-multiplier
            HBox matchRow = new HBox(matchLabel, spacingRegion, multiplierLabel);
            matchRow.setAlignment(Pos.CENTER);
            matchRow.setSpacing(10);

            // margin for the row
            VBox.setMargin(matchRow, new Insets(5, 5, 5, 5));

            slip.getChildren().addAll(matchRow);
        }
        Button addButton = new Button("Add");
        addButton.getStyleClass().add("right-buttons");
        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(2);
        slip.getChildren().addAll(addButton, bottomSpacer);

        //close the window opened when press on the button
        if(!f){
            addButton.setDisable(true);
        }
        addButton.setOnAction(event -> {
            if (live.getDialog() != null && live.getDialog().isShowing()) {

                SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
                Bet bet = new Bet();
                String[] values = concat.split("_");

                //we access to the single values
                String value1 = values[0]; // "1X"
                String value2 = values[1]; // "2.1"
                String value3 = values[2]; // "Milan"
                String value4 = values[3]; // "Inter"
                String value5 = values[4]; // "2021-01-01"

                bet.setChosenMultiplierName(value1);
                //multiplier values have the comma, but a double needs the dot, so we change the comma in dot
                String changeComma = value2.replace(',', '.');
                bet.setChosenMultiplierValue(Double.parseDouble(changeComma));
                bet.setTeamHome(value3);
                bet.setTeamAway(value4);
                bet.setMatchDate(value5);
                bet.setCompetition_id("null");
                bet.setMatchID(0);
                bet.setWin(-1);
                int x = slipRedisDAO.addBetToSlip(Session.getUsername(), id, bet);
                // Send an error message because the match is started.
                if (x == 1) {
                    Label errorLabel = new Label("This match is now in play");
                    errorLabel.getStyleClass().add("error");
                    slip.getChildren().add(errorLabel);
                    addButton.setDisable(true);
                }else{
                    live.getDialog().close();
                }
                slipRedisDAO.refreshTTL(Session.getUsername(), id.toString());
            }

        });

        return slip;
    }
}