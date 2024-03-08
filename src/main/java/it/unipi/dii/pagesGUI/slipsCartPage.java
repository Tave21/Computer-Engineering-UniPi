package it.unipi.dii.pagesGUI;

import it.unipi.dii.dao.mongo.CustomerMongoDBDAO;
import it.unipi.dii.dao.redis.SlipRedisDAO;
import it.unipi.dii.model.Bet;
import it.unipi.dii.model.Slip;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.function.UnaryOperator;

public class slipsCartPage {
    private Label valueLabel = new Label();
    private VBox creditColumn;
    private VBox slipsCartColumn;
    private List<totalValueLabel> totalValueLabelList = new ArrayList<>();
    private double userCredit;

    public StackPane getContent(){

        StackPane stackPane = new StackPane();

        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(50);

        Label titleLabel = new Label("Slips' Cart");
        titleLabel.getStyleClass().add("live-title");

        slipsCartColumn = createSlipsColumn("Slips");
        slipsCartColumn.setMaxWidth(700);
        CustomerMongoDBDAO customerMongoDBDAO = new CustomerMongoDBDAO();
        customerMongoDBDAO.openConnection();
        userCredit = customerMongoDBDAO.getCreditOfCustomer(Session.getUsername()); //take credit of specific user
        customerMongoDBDAO.closeConnection();
        creditColumn = createCreditColumn("Credit");

        HBox slipsContent = new HBox();
        slipsContent.setAlignment(Pos.CENTER);
        slipsContent.setSpacing(20);
        slipsContent.getChildren().addAll(slipsCartColumn, creditColumn);

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
    protected VBox createCreditColumn(String columnTitle) {

        VBox column = new VBox();
        column.setAlignment(Pos.TOP_CENTER);
        Label columnTitleLabel = new Label(columnTitle);
        columnTitleLabel.getStyleClass().add("input-label");

        VBox.setMargin(columnTitleLabel, new Insets(0, 0, 10, 0));
        columnTitleLabel.setAlignment(Pos.CENTER);
        column.getChildren().add(columnTitleLabel);

        VBox creditRow = createCredit();
        VBox.setMargin(creditRow, new Insets(0, 0, 10, 0)); //margin, to distanciate all slips
        column.getChildren().addAll(creditRow);

        return column;
    }
    protected VBox createCredit() {
        // Creazione di un singolo slip
        VBox creditBox = new VBox();
        creditBox.getStyleClass().addAll("form", "form-container");
        creditBox.setAlignment(Pos.CENTER);
        creditBox.setSpacing(10);

        Label creditLabel = new Label("Your Credit: ");
        creditLabel.getStyleClass().add("input-label");

        //Label valueLabel = new Label("");
        valueLabel.setText(Double.toString(userCredit));
        valueLabel.getStyleClass().add("input-label");

        Region spacingRegion = new Region();
        HBox.setHgrow(spacingRegion, Priority.ALWAYS);
        HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 25));

        HBox amountRow = new HBox();
        amountRow.setAlignment(Pos.CENTER);
        amountRow.setSpacing(10);
        VBox.setMargin(amountRow, new Insets(5, 5, 5, 5));

        amountRow.getChildren().addAll(creditLabel, spacingRegion, valueLabel);

        Label amountLabel = new Label("Add Credit:");
        amountLabel.getStyleClass().add("input-label");

        TextField amountField = new TextField();
        amountField.setPrefWidth(40);
        amountField.getStyleClass().add("amount-field");
        amountField.setText("2");

        Label moneyLabel = new Label("€");
        moneyLabel.getStyleClass().add("input-label");

        Button addButton = new Button("Add");
        addButton.getStyleClass().add("right-buttons");

        Region spacingRegion1 = new Region();
        HBox.setHgrow(spacingRegion1, Priority.ALWAYS);
        HBox.setMargin(spacingRegion1, new Insets(0, 0, 0, 55));

        HBox addRow = new HBox();

        addRow.setAlignment(Pos.CENTER);
        addRow.setSpacing(10);
        VBox.setMargin(addRow, new Insets(5, 5, 5, 5));
        addRow.getChildren().addAll(amountLabel, amountField, moneyLabel, addButton);

        Label errorLabel = new Label("");
        errorLabel.getStyleClass().add("input-label");

        HBox errorBox = new HBox();
        errorBox.setAlignment(Pos.CENTER);
        errorBox.getChildren().addAll(errorLabel);

        UnaryOperator<TextFormatter.Change> filter = change -> {
            String newText = change.getControlNewText();
            if (newText.matches("\\d*")) {
                int newValue = newText.isEmpty() ? 0 : Integer.parseInt(newText);
                if (newValue <= 500) {
                    return change;
                }
            }
            return null;
        };
        TextFormatter<String> textFormatter = new TextFormatter<>(filter);
        amountField.setTextFormatter(textFormatter);

        addButton.setOnAction(event -> handleAddButtonClick(
                amountField,
                errorLabel
        ));

        creditBox.getChildren().addAll(amountRow, errorBox, addRow);

        return creditBox;
    }
    protected VBox createSlipsColumn(String columnTitle) {
        VBox column = new VBox();
        column.setAlignment(Pos.TOP_CENTER);
        Label columnTitleLabel = new Label(columnTitle);
        columnTitleLabel.getStyleClass().add("input-label");

        VBox.setMargin(columnTitleLabel, new Insets(0, 0, 10, 0));
        columnTitleLabel.setAlignment(Pos.CENTER);
        column.getChildren().add(columnTitleLabel);

        SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
        List<Slip> list = new ArrayList<>();
        try {
            list = slipRedisDAO.getListFromUser(Session.getUsername());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        if(list.size() == 0){
            Label emptyCartLabel = new Label("The are not slips in the cart");
            emptyCartLabel.getStyleClass().add("input-label");
            column.getChildren().add(emptyCartLabel);
        }
        for (Slip s : list){
            String[] matchArray = new String[s.findBetsList().size()];
            String[] multiplierArray = new String[s.findBetsList().size()];
            int i = 0;
            for (Bet b : s.findBetsList()){
                matchArray[i] = b.getTeamHome() + "-" + b.getTeamAway();
                multiplierArray[i] = b.getChosenMultiplierName() + "   " + b.getChosenMultiplierValue();
                i++;
            }
            VBox amountRow = createAmountColumn(multiplierArray, s.getSlipID());
            VBox slip = createSlip(matchArray, multiplierArray, amountRow, s.getSlipID());
            VBox.setMargin( slip, new Insets(0, 0, 10, 0)); //margin, to distanciate all slips
            column.getChildren().add(slip);
        }

        return column;
    }
    protected VBox createSlip(String[] match, String[] multiplier, VBox amountRow, Integer id) {
        VBox slip = new VBox();
        slip.getStyleClass().addAll("form1", "form-container1");
        slip.setAlignment(Pos.CENTER);
        slip .setSpacing(10);

        for (int i = 0; i < match.length; i++) {
            Label matchLabel = new Label(match[i]);
            matchLabel.getStyleClass().add("input-label");

            Label multiplierLabel = new Label(multiplier[i]);
            multiplierLabel.getStyleClass().add("input-label");

            Button deleteButton = new Button("Delete");
            deleteButton.getStyleClass().add("right-buttons");

            Region spacingRegion = new Region();
            HBox.setHgrow(spacingRegion, Priority.ALWAYS);
            HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 55));

            HBox matchRow = new HBox();

            matchRow.getChildren().addAll(matchLabel, spacingRegion, multiplierLabel, deleteButton);
            //action when we click on a delete button of a single match
            deleteButton.setOnAction(event -> {
                int rowIndex = slip.getChildren().indexOf(matchRow);
                slip.getChildren().remove(matchRow);

                String[] values = matchLabel.getText().split("-");
                System.out.println(matchLabel);
                String teamHome = values[0];
                String teamAway = values[1];

                String[] val = multiplierLabel.getText().split("   ");
                System.out.println(multiplierLabel);
                String nameMult= val[0];
                String valMult= val[1];
                SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
                Bet bet = new Bet();
                bet.setMatchID(0);
                bet.setCompetition_id("null");
                bet.setChosenMultiplierName(nameMult);
                bet.setChosenMultiplierValue(Double.parseDouble(valMult));
                bet.setTeamAway(teamAway);
                bet.setTeamHome(teamHome);
                bet.setWin(-1);
                slipRedisDAO.deleteBetFromSlip(Session.getUsername(), id, bet);

                //if there is only a bet, remove the entire slip
                if (slip.getChildren().size() == 1) {
                    //remove the slip from redis
                    slipRedisDAO.delete_Slip(Session.getUsername(), id);
                    try {
                        //if the cart is empty tell it to the user
                        if(slipRedisDAO.getListFromUser(Session.getUsername()).size() == 0){
                            Label emptyCartLabel = new Label("The are not slips in the cart");
                            emptyCartLabel.getStyleClass().add("input-label");
                            ((VBox) slip.getParent()).getChildren().add(emptyCartLabel);
                        }
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                    //remove slip from the cart
                    ((VBox) slip.getParent()).getChildren().remove(slip);

                    //remove the total label from the list
                    int index = getPositionById(totalValueLabelList, id);
                    totalValueLabelList.remove(index);


                }else {
                    slipRedisDAO.refreshTTL(Session.getUsername(), Integer.toString(id));
                    //inside the else otherwise give the out of bound index
                    int index = getPositionById(totalValueLabelList, id);
                    Label totalValueLabel = totalValueLabelList.get(index).getTotalLabel();
                    double removedMultiplier = Double.parseDouble(valMult);
                    String oldTotal = totalValueLabel.getText().replace(",", ".");
                    double oldTotalAmount = Double.parseDouble(oldTotal);

                    //totalValueLabel.setText(String.valueOf(oldTotalValue/removedMultiplier));
                    double oldTotalValue = totalValueLabelList.get(index).getMultiplierProduct();
                    //double multipliers = oldTotalValue / removedMultiplier;
                    //totalValueLabelList.get(index).setMultiplierProduct(multipliers);
                    double newValue1 = (oldTotalAmount / oldTotalValue);
                    double multipliers = oldTotalValue / removedMultiplier;
                    totalValueLabelList.get(index).setMultiplierProduct(multipliers);
                    //System.out.println("multiplier aggiornato" +  totalValueLabelList.get(index).getMultiplierProduct());
                    //System.out.println("betAmount: " +  newValue1);
                    int newValue = (int) Math.round(newValue1);
                    //System.out.println("betAmount round: " +  newValue);
                    updateTotalValueLabel(totalValueLabelList.get(index).getTotalLabel(), newValue, multipliers);
                }
            });

            matchRow.setAlignment(Pos.CENTER);
            matchRow.setSpacing(10);

            VBox.setMargin(matchRow, new Insets(5, 5, 5, 5));

            slip.getChildren().addAll(matchRow);
        }

        VBox.setMargin(amountRow, new Insets(5, 5, 5, 5));
        slip.getChildren().addAll(amountRow);

        return slip;
    }
    protected VBox createAmountColumn(String[] multiplier, Integer id) {

        VBox amountColumn = new VBox();
        amountColumn.setAlignment(Pos.CENTER);
        amountColumn.setSpacing(10);

        Label errorLabel = new Label("");
        errorLabel.getStyleClass().add("input-label");
        //these are buttons that are hidden, we use them only to store data

        HBox errorBox = new HBox();
        errorBox.setAlignment(Pos.CENTER);
        errorBox.getChildren().addAll(errorLabel);

        HBox amountRow = new HBox();
        amountRow.setAlignment(Pos.CENTER);
        amountRow.setSpacing(10);

        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);

        Label amountLabel = new Label("Amount:");
        amountLabel.getStyleClass().add("input-label");

        TextField amountField = new TextField();
        amountField.setPrefWidth(40);
        amountField.getStyleClass().add("amount-field");
        amountField.setText("2");

        Label totalLabel = new Label("Total:");
        totalLabel.getStyleClass().add("input-label");

        Label moneyLabel = new Label("€");
        moneyLabel.getStyleClass().add("input-label");
        HBox.setMargin(moneyLabel, new Insets(0, 0, 0, 5));

        Label totalValueLabel = new Label("0.0");
        totalValueLabel.getStyleClass().add("input-label");
        totalValueLabel totalValue = new totalValueLabel(id, totalValueLabel);
        totalValueLabelList.add(totalValue);
        int index = getPositionById(totalValueLabelList, id);

        Button payButton = new Button("Pay");
        payButton.getStyleClass().add("right-buttons");

        double val=1;
        for (int i = 0; i < multiplier.length; i++) {
            String[] mul = multiplier[i].split("\\s{3}");
            val *= Double.parseDouble(mul[1]);
        }

        payButton.setUserData(val);
        double retrievedValue = (double) payButton.getUserData();
        //we set the betAmount
        totalValueLabelList.get(index).setMultiplierProduct(retrievedValue);

        Button deleteButton = new Button("Delete");
        deleteButton.getStyleClass().add("right-buttons");
        //at the beginnning, the total amount is visualized in the cart
        int betAmount = Integer.parseInt(amountField.getText());
        updateTotalValueLabel(totalValueLabelList.get(index).getTotalLabel(), betAmount, retrievedValue);
        HBox.setMargin(amountRow, new Insets(5, 5, 5, 5));

        /*
        UnaryOperator<TextFormatter.Change> filter = change -> {
            String newText = change.getControlNewText();
            if (newText.matches("\\d*")) {
                int newValue = newText.isEmpty() ? 0 : Integer.parseInt(newText);
                if (newValue <= 200) {
                    int index1 = getPositionById(totalValueLabelList, id);
                    double retrievedValue1 = totalValueLabelList.get(index1).getMultiplierProduct();
                    updateTotalValueLabel(totalValueLabelList.get(index1).getTotalLabel(), newValue, retrievedValue1);
                    System.out.println("unary operator");
                    return change;
                }
            }
            return null;
        };
        TextFormatter<String> textFormatter = new TextFormatter<>(filter);
        amountField.setTextFormatter(textFormatter);

         */

        amountField.textProperty().addListener((observable, oldValue, newValue) -> {
            String regex = "^(2[0-9]?|[3-9][0-9]?|1[0-9]{1,2}|200)$";
            boolean isMatch = amountField.getText().matches(regex);
            int field;
            if(isMatch){
                field = Integer.parseInt(amountField.getText());
            }else{
                field = 0;
            }
            int index1 = getPositionById(totalValueLabelList, id);
            double retrievedValue1 = totalValueLabelList.get(index1).getMultiplierProduct();
            //System.out.println("prodotto ricavato quando cambia l'amount: "+ totalValueLabelList.get(index1).getMultiplierProduct());
            updateTotalValueLabel(totalValueLabelList.get(index1).getTotalLabel(), field, retrievedValue1);
        });



        payButton.setOnAction(event -> handlePayButtonClick(
                amountField,
                payButton,
                errorLabel,
                //totalValueLabelList.get(index).getTotalLabel(),
                //totalValueLabelList.get(index).getMultiplierProduct(),
                id
        ));

        deleteButton.setOnAction(event -> handleDeleteButtonClick(deleteButton, id));

        amountRow.getChildren().addAll(amountLabel, amountField, moneyLabel, totalLabel, totalValueLabelList.get(index).getTotalLabel(), payButton, deleteButton);

        amountColumn.getChildren().addAll(errorBox, amountRow);

        return amountColumn;
    }
    private void handlePayButtonClick(TextField amountField, Button payButton,  Label errorLabel, Integer id) {
        //we get the new index of the correct label
        int index = getPositionById(totalValueLabelList, id);
        Label totalValueLabel = totalValueLabelList.get(index).getTotalLabel();
        double retrievedValue = totalValueLabelList.get(index).getMultiplierProduct();
        String amountText = amountField.getText();
        Double amount = 0.0;
        /*
        if amount is not empty convert in Double otherwise enter inside the if
        it creates an error if we convert the amount if the user doesn't insert a value in the field
        regular expression to limit a value between 2 and 200
        */

        String regex = "^(2[0-9]?|[3-9][0-9]?|1[0-9]{1,2}|200)$";
        boolean isMatch = amountText.matches(regex);
        if (!amountText.isEmpty() && isMatch) {
            amount = Double.parseDouble(amountText);
        }

        System.out.println("amount = "+ amount+ " userCredit = "+ userCredit);
        if (/*amountText.isEmpty()  || amountText.equals("0") || amountText.equals("1") ||*/ !isMatch) {
            errorLabel.setText("Enter a number between 2 and 200");
            errorLabel.getStyleClass().add("error");
        }else if(amount > userCredit) {
            errorLabel.setText("Insufficient Credit");
            errorLabel.getStyleClass().add("error");

        }else {
            errorLabel.setText("");
            updateTotalValueLabel(totalValueLabel, Integer.parseInt(amountText), retrievedValue);
            // remove slips from column "Slips' Cart"
            Node currentParent = payButton.getParent();
            while (currentParent != null && !(currentParent instanceof VBox)) {
                currentParent = currentParent.getParent().getParent();
            }

            if (currentParent != null) {
                VBox parentVBox = (VBox) currentParent;
                ((VBox)  parentVBox.getParent()).getChildren().remove( parentVBox);
            }
            userCredit-=amount;
            valueLabel.setText(Double.toString(userCredit));

            SlipRedisDAO slipRedisDAO = new SlipRedisDAO();

            CustomerMongoDBDAO customerMongoDBDAO= new CustomerMongoDBDAO();
            customerMongoDBDAO.openConnection();
            boolean t = customerMongoDBDAO.pay(Session.getUsername(), amount); //remove credit from user
            if(t){
                slipRedisDAO.sendConfirmedSlipToMongo(Session.getUsername(),id,amount );
            }
            customerMongoDBDAO.closeConnection();

            //remove the total label from the list
            int index1 = getPositionById(totalValueLabelList, id);
            totalValueLabelList.remove(index1);

        }
    }

    private void handleAddButtonClick(TextField amountField, Label errorLabel) {
        String amountText = amountField.getText();

        if (amountText.isEmpty()  || amountText.equals("0") || amountText.equals("1")) {
            errorLabel.setText("Enter a number between 2 and 500");
            errorLabel.getStyleClass().add("error");
        } else {
            errorLabel.setText("");
            userCredit += Integer.parseInt(amountText);
            valueLabel.setText(Double.toString(userCredit));
            CustomerMongoDBDAO customerMongoDBDAO= new CustomerMongoDBDAO();
            customerMongoDBDAO.openConnection();
            customerMongoDBDAO.redeem(Session.getUsername(), Integer.parseInt(amountText)); //add credit to user
            customerMongoDBDAO.closeConnection();
        }
    }
    private void handleDeleteButtonClick(Button deleteButton, Integer id) {
        // remove slips from column "Slips' Cart"
        Node currentParent = deleteButton.getParent();
        while (currentParent != null && !(currentParent instanceof VBox)) {
            currentParent = currentParent.getParent().getParent();
        }

        if (currentParent != null) {
            VBox parentVBox = (VBox) currentParent;
            //remove the slip from redis
            SlipRedisDAO slipRedisDAO = new SlipRedisDAO();
            slipRedisDAO.delete_Slip(Session.getUsername(), id);

            try {
                // If the cart is empty tell it to the user.
                if(slipRedisDAO.getListFromUser(Session.getUsername()).size() == 0){
                    Label emptyCartLabel = new Label("The are not slips in the cart");
                    emptyCartLabel.getStyleClass().add("input-label");
                    ((VBox)parentVBox.getParent()).getChildren().add(emptyCartLabel);
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            // Remove the slip from the cart.
            ((VBox)parentVBox.getParent()).getChildren().remove( parentVBox);
        }
        // Remove the total label from the list.
        int index = getPositionById(totalValueLabelList, id);
        totalValueLabelList.remove(index);
    }
    private void updateTotalValueLabel(Label totalValueLabel, int newValue, double multipliers) {
        Double calculatedValue = newValue * (multipliers);
        String formattedValue = String.format("%.2f", calculatedValue);
        totalValueLabel.setText(formattedValue);
    }

    public int getPositionById(List<totalValueLabel> list, int desiredId) {
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).getSlipID() == desiredId) {
                return i;
            }
        }
        return -1;
    }
}