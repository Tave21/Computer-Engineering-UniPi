package it.unipi.dii.pagesGUI;

import com.mongodb.client.AggregateIterable;
import it.unipi.dii.dao.mongo.CustomerMongoDBDAO;
import it.unipi.dii.dto.CustomerDTO;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import static it.unipi.dii.utility.converters.jsonToDocumentConverter.convertDocumentToJson;
import static it.unipi.dii.utility.converters.jsonToObjectConverter.convertJsonToObject;

public class customerPage {
    private List<CustomerDTO> userList;
    private TextField usernameField;
    private TextField nameField;
    private TextField surnameField;

    public StackPane getContent() {
        this.userList = new ArrayList<>();
        this.usernameField = new TextField();
        this.nameField = new TextField();
        this.surnameField = new TextField();

        StackPane stackPane = new StackPane();

        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(20);

        VBox content = new VBox();
        content.setAlignment(Pos.CENTER);
        content.setSpacing(20);
        VBox.setVgrow(content, Priority.ALWAYS);

        Label titleLabel = new Label("Find customers");
        titleLabel.getStyleClass().add("live-title");
        // Creation of the input fields and research button.
        Label username = new Label("Username");
        username.getStyleClass().add("input-label");
        Label name = new Label("Name");
        name.getStyleClass().add("input-label");
        Label surname = new Label("Surname");
        surname.getStyleClass().add("input-label");


        VBox usernameBox = new VBox();
        usernameBox.getChildren().addAll(username, usernameField);
        VBox.setVgrow(usernameBox, Priority.ALWAYS);

        VBox nameBox = new VBox();
        nameBox.getChildren().addAll(name, nameField);
        VBox.setVgrow(nameBox, Priority.ALWAYS);

        VBox surnameBox = new VBox();
        surnameBox.getChildren().addAll(surname, surnameField);
        VBox.setVgrow(surnameBox, Priority.ALWAYS);

        HBox lineBox = new HBox();
        HBox.setHgrow(lineBox, Priority.ALWAYS);

        HBox.setMargin(lineBox, new Insets(15, 15, 15, 15));
        lineBox.setSpacing(10);
        lineBox.setAlignment(Pos.CENTER);
        lineBox.getChildren().addAll(usernameBox, nameBox, surnameBox);

        // Creation of output area for found users.
        VBox outputArea = new VBox();
        outputArea.setSpacing(10);
        VBox.setVgrow(outputArea, Priority.ALWAYS);

        // Action when fields are modified.
        usernameField.textProperty().addListener((observable, oldValue, newValue) -> updateFilteredUsers(outputArea));

        nameField.textProperty().addListener((observable, oldValue, newValue) -> updateFilteredUsers(outputArea));

        surnameField.textProperty().addListener((observable, oldValue, newValue) -> updateFilteredUsers(outputArea));

        Region formSpacer = new Region();
        formSpacer.setPrefHeight(10);

        Region midSpacer = new Region();
        midSpacer.setPrefHeight(10);

        VBox body = new VBox();
        body.getStyleClass().addAll("form1", "form-container1");
        body.getChildren().addAll(formSpacer, lineBox, midSpacer, outputArea, bottomSpacer);
        // Expands body and occupy all available space.
        VBox.setVgrow(body, Priority.ALWAYS);

        content.getChildren().addAll(topSpacer, titleLabel, body);
        content.setPadding(new Insets(10, 10, 10, 10));

        stackPane.getChildren().addAll(content);
        ScrollPane scrollPane = new ScrollPane(stackPane);
        scrollPane.setFitToWidth(true);
        scrollPane.getStyleClass().add("matches_scroll");

        VBox.setVgrow(scrollPane, Priority.ALWAYS);

        return new StackPane(scrollPane);
    }

    private void initializeUserList(String name, String surname, String username) {
        userList.clear();

        if (!username.isEmpty() || !name.isEmpty() || !surname.isEmpty()) {

            CustomerMongoDBDAO cs = new CustomerMongoDBDAO();
            cs.openConnection();
            List<Document> regExps = new ArrayList<>();

            if (!username.isEmpty()) {
                regExps.add(new Document("username", new Document("$regex", Pattern.compile("^" + username + "(?i)"))));
            }

            if (!name.isEmpty()) {
                regExps.add(new Document("name", new Document("$regex", Pattern.compile("^" + name + "(?i)"))));
            }

            if (!surname.isEmpty()) {
                regExps.add(new Document("surname", new Document("$regex", Pattern.compile("^" + surname + "(?i)"))));
            }

            List<Document> pipeline = Arrays.asList(new Document("$match",
                            new Document("$and",regExps)),
                    new Document("$project",
                            new Document("_id", 0L)
                                    .append("name", 1L)
                                    .append("surname", 1L)
                                    .append("username", 1L)),
                    new Document("$limit", 20L));



            AggregateIterable<Document> docs = cs.mongoDB.getCollection("customers").aggregate(pipeline);
            for (Document document : docs) {
                userList.add(convertJsonToObject(convertDocumentToJson(document), CustomerDTO.class));
            }
        }
    }

    private void updateFilteredUsers(VBox outputArea) {
        String username = usernameField.getText().toLowerCase();
        String name = nameField.getText().toLowerCase();
        String surname = surnameField.getText().toLowerCase();

        //filter users with new values of input fields
        List<CustomerDTO> filteredUsers = filterUsers(username, name, surname);

        if (!filteredUsers.isEmpty() && (!username.isEmpty() || !name.isEmpty() || !surname.isEmpty())) {
            showUsers(outputArea, filteredUsers);
        } else {
            //if all fields are empty, clear the output area
            userList.clear();
            outputArea.getChildren().clear();
        }
    }

    private List<CustomerDTO> filterUsers(String username, String name, String surname) {
        List<CustomerDTO> filteredUsers = new ArrayList<>();
        initializeUserList(name, surname, username);

        if(!userList.isEmpty()) {

            for (CustomerDTO user : userList) {
                if ((username.isEmpty() || user.getUsername().toLowerCase().startsWith(username)) &&
                        (name.isEmpty() || user.getName().toLowerCase().startsWith(name)) &&
                        (surname.isEmpty() || user.getSurname().toLowerCase().startsWith(surname))) {
                    filteredUsers.add(user);
                }
            }
        }

        return filteredUsers;
    }

    private void showUsers(VBox outputArea, List<CustomerDTO> users) {
        //clear the output area
        outputArea.getChildren().clear();

        //show users in the VBox output.
        for (CustomerDTO user : users) {
            HBox userBox = new HBox(10);
            HBox.setHgrow(userBox, Priority.ALWAYS);
            Label usernameLabel = new Label("Username:");
            usernameLabel.getStyleClass().add("input-label");
            usernameLabel.setMaxWidth(700);
            Label username = new Label(user.getUsername());
            username.getStyleClass().add("user-label");
            username.setMaxWidth(700);

            Label nameLabel = new Label("Name:");
            nameLabel.getStyleClass().add("input-label");
            nameLabel.setMaxWidth(700);
            Label name = new Label(user.getName());
            name.getStyleClass().add("user-label");
            name.setMaxWidth(700);

            Label surnameLabel = new Label("Surname:");
            surnameLabel.getStyleClass().add("input-label");
            surnameLabel.setMaxWidth(700);
            Label surname = new Label(user.getSurname());
            surname.getStyleClass().add("user-label");
            surname.setMaxWidth(700);

            Button deleteButton = new Button("Delete");

            deleteButton.getStyleClass().add("right-buttons");
            deleteButton.setOnAction(event -> {
                //implement action for the deletion
                userList.remove(user);

                CustomerMongoDBDAO cs = new CustomerMongoDBDAO();
                cs.openConnection();
                cs.removeCustomer(user.getUsername());
                cs.closeConnection();

                //after the user is removed, show again the new list
                showUsers(outputArea, filterUsers(usernameField.getText().toLowerCase(),
                        nameField.getText().toLowerCase(),
                        surnameField.getText().toLowerCase()));
            });
            userBox.setAlignment(Pos.CENTER);

            userBox.getChildren().addAll(usernameLabel, username, nameLabel, name, surnameLabel, surname, deleteButton);
            outputArea.getChildren().add(userBox);
        }
    }
}