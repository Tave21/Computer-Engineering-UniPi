package it.unipi.dii.pagesGUI;

import com.mongodb.client.AggregateIterable;
import it.unipi.dii.dao.mongo.SlipMongoDBDAO;
import it.unipi.dii.model.Slip;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static it.unipi.dii.utility.JsonToDocument.convertDocumentToJson;
import static it.unipi.dii.utility.JsonToObjectConverter.convertJsonToObject;

public class confirmedSlipsPage {

    private VBox confirmedSlipsColumn;
    List<String> stateLabel = new ArrayList<>();

    public StackPane getContent() {

        StackPane stackPane = new StackPane();

        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(50);

        Label titleLabel = new Label("Confirmed Slips");
        titleLabel.getStyleClass().add("live-title");

        confirmedSlipsColumn = createSlipsColumn();

        HBox slipsContent = new HBox();
        slipsContent.setAlignment(Pos.CENTER);
        slipsContent.setSpacing(20);
        slipsContent.getChildren().addAll(confirmedSlipsColumn);

        VBox generalContent = new VBox();
        generalContent.setAlignment(Pos.CENTER);
        generalContent.setSpacing(20);
        generalContent.getChildren().addAll(topSpacer, titleLabel, slipsContent, bottomSpacer);

        stackPane.getChildren().addAll(generalContent);
        ScrollPane scrollPane = new ScrollPane(stackPane);
        scrollPane.setFitToWidth(true);
        scrollPane.getStyleClass().add("matches_scroll");

        VBox.setVgrow(scrollPane, Priority.ALWAYS);

        return new StackPane(scrollPane);
    }

    protected VBox createSlipsColumn() {
        //column creation for the slips
        VBox column = new VBox();
        column.setAlignment(Pos.TOP_CENTER);

        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("username", Session.getUsername())),
                new Document("$project",
                        new Document("_id", 0L)),
                new Document("$sort",
                        new Document("confirmationDate", -1L)));

        SlipMongoDBDAO slipMongoDBDAO = new SlipMongoDBDAO();
        slipMongoDBDAO.openConnection();
        AggregateIterable<Document> docs = slipMongoDBDAO.mongoDB.getCollection("slips").aggregate(pipeline);
        int p = 0;
        List<Slip> slips = new ArrayList<>();
        for (Document document : docs) {
            slips.add(convertJsonToObject(convertDocumentToJson(document), Slip.class));

            //set the win value for each bet inside a slip
            List<Document> betsList = document.getList("betsList", Document.class);

            int x = 0;
            for (Document betDocument : betsList) {
                int winValue = betDocument.getInteger("win", -1); // Default a -1 se non è presente
                slips.get(p).findBetsList().get(x).setWin(winValue);
                x++;
            }
            p++;
        }

        slipMongoDBDAO.closeConnection();

        int j = 0;
        for (Slip slip : slips) {

            String[] matchArray = new String[slip.findBetsList().size()];
            String[] multiplierArray = new String[slip.findBetsList().size()];
            int[] winBet = new int[slip.findBetsList().size()];
            for (int i = 0; i < slip.findBetsList().size(); i++) {
                matchArray[i] = slip.findBetsList().get(i).getTeamHome() + "-" + slip.findBetsList().get(i).getTeamAway();
                String correct_multiplier = String.format("%.1f", slip.findBetsList().get(i).getChosenMultiplierValue());
                multiplierArray[i] = slip.findBetsList().get(i).getChosenMultiplierName() + "   " + correct_multiplier;
                //System.out.println("prima del for è : "+ slip.findBetsList().get(i).getWin());
                winBet[i] = slip.findBetsList().get(i).getWin();
                //System.out.println(slip.findBetsList().get(i).getWin());
                //System.out.println("nel for è : "+ winBet[i]);
            }
            String creationDate = slip.getCreationDate();
            double Betamount = slip.getBetAmount();
            slip.computeTotal();
            double Amount = slip.getAmount();
            Integer state = slip.getWin();
            if (state == -1) {
                stateLabel.add("in progress");
            } else if (state == 1) {
                stateLabel.add("won");
            } else {
                stateLabel.add("lost");
            }
            VBox confirmedSlip = createSlip(matchArray, multiplierArray, winBet, creationDate, Betamount, Amount, stateLabel.get(j));
            VBox.setMargin(confirmedSlip, new Insets(0, 0, 10, 0));
            column.getChildren().add(confirmedSlip);
            j++;
        }


        return column;
    }

    protected VBox createSlip(String[] match, String[] multiplier, int[] winBet, String creationDate, double amount, double total, String state) {
        //single slip creation
        VBox slip = new VBox();
        slip.getStyleClass().addAll("form", "form-container-slips");
        slip.setAlignment(Pos.CENTER);
        slip.setSpacing(10);

        for (int i = 0; i < match.length; i++) {

            Label matchLabel = new Label(match[i]);
            Label multiplierLabel = new Label(multiplier[i]);
            //System.out.println("win bet = "+ winBet[i]);
            if(winBet[i] == 1){
                //if the user won that bet, the label is green
                matchLabel.getStyleClass().add("input-label");
                multiplierLabel.getStyleClass().add("input-label");
            }else if(winBet[i] == -1){
                //the match has not been played yet
                //System.out.println("match not played yet");
                matchLabel.getStyleClass().add("progress");
                multiplierLabel.getStyleClass().add("progress");
            }else{
                //otherwise red label
                matchLabel.getStyleClass().add("error");
                multiplierLabel.getStyleClass().add("error");
            }

            Region spacingRegion = new Region();
            HBox.setHgrow(spacingRegion, Priority.ALWAYS);
            HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 55));

            HBox matchRow = new HBox();

            matchRow.getChildren().addAll(matchLabel, spacingRegion, multiplierLabel);

            matchRow.setAlignment(Pos.CENTER);
            matchRow.setSpacing(10);

            VBox.setMargin(matchRow, new Insets(5, 5, 5, 5));

            slip.getChildren().addAll(matchRow);
        }

        Label dateLabel = new Label("Creation Date: ");
        dateLabel.getStyleClass().add("user-label");
        String correctDate = creationDate.replace("T", " ");
        correctDate = correctDate.replace("Z", "");
        Label creationDateLabel = new Label(correctDate);
        creationDateLabel.getStyleClass().add("user-label");
        Region spacingRegion = new Region();
        HBox.setHgrow(spacingRegion, Priority.ALWAYS);
        HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 55));

        HBox dateRow = new HBox();

        dateRow.getChildren().addAll(dateLabel, spacingRegion, creationDateLabel);

        dateRow.setAlignment(Pos.CENTER);
        dateRow.setSpacing(10);
        VBox.setMargin(dateRow, new Insets(5, 5, 5, 5));

        Label amountLabel = new Label("Amount: ");
        amountLabel.getStyleClass().add("user-label");
        Label amountValueLabel = new Label(amount + " €");
        amountValueLabel.getStyleClass().add("user-label");

        Label totalLabel = new Label("Total: ");
        totalLabel.getStyleClass().add("user-label");
        String correct_total = String.format("%.2f", total);
        Label totalValueLabel = new Label(correct_total + " €");
        totalValueLabel.getStyleClass().add("user-label");

        Region spacingRegion1 = new Region();
        HBox.setHgrow(spacingRegion1, Priority.ALWAYS);
        HBox.setMargin(spacingRegion1, new Insets(0, 0, 0, 55));

        HBox amountRow = new HBox();

        amountRow.getChildren().addAll(amountLabel, amountValueLabel, spacingRegion1, totalLabel, totalValueLabel);

        amountRow.setAlignment(Pos.CENTER);
        amountRow.setSpacing(10);

        VBox.setMargin(amountRow, new Insets(5, 5, 5, 5));

        Label stateLabel = new Label("State: ");
        stateLabel.getStyleClass().add("user-label");
        Label stateValueLabel = new Label(state);
        stateValueLabel.getStyleClass().add("user-label");
        Region spacingRegion2 = new Region();
        HBox.setHgrow(spacingRegion2, Priority.ALWAYS);
        HBox.setMargin(spacingRegion2, new Insets(0, 0, 0, 55));

        HBox stateRow = new HBox();

        stateRow.getChildren().addAll(stateLabel, spacingRegion2, stateValueLabel);

        stateRow.setAlignment(Pos.CENTER);
        stateRow.setSpacing(10);

        VBox.setMargin(stateRow, new Insets(5, 5, 5, 5));

        slip.getChildren().addAll(dateRow, amountRow, stateRow);

        return slip;
    }
}