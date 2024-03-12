package it.unipi.dii.pagesGUI;

import it.unipi.dii.BeansBetGUI;
import it.unipi.dii.HomeAdmin;
import it.unipi.dii.HomeRegistered;
import it.unipi.dii.dao.mongo.AdminMongoDBDAO;
import it.unipi.dii.dao.mongo.CustomerMongoDBDAO;
import it.unipi.dii.dao.redis.PollRedisDAO;
import it.unipi.dii.userCookie.customerInfo;
import it.unipi.dii.utility.JsonToObjectConverter;
import javafx.application.Platform;
import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.layout.*;
import javafx.geometry.Pos;

import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.userCookie.usernameCookie.createUserCookie;
import static it.unipi.dii.utility.JsonToObjectConverter.convertJsonToObject;

public class loginPage {
    private BeansBetGUI beansBetGUI;
    private Label errorLabel;
    private List<Label> additionalLabels;
    public loginPage(BeansBetGUI beansBetGUI) {
        this.beansBetGUI = beansBetGUI;
        this.errorLabel = new Label("");
    }
    public HBox getHeader() {
        navBar buttonHeader = new navBar(beansBetGUI);
        HBox header = new HBox();
        return buttonHeader.createHeader(header, 1);
    }
    public VBox getContent() {
        this.additionalLabels = new ArrayList<>();
        VBox loginContent = new VBox();
        Region spacer = new Region();
        spacer.setPrefHeight(47);
        loginContent.getChildren().add(spacer);

        // Components for the login form.
        TextField usernameField = new TextField();
        usernameField.setMaxWidth(200);

        // Get the value from the input fields (password and username).
        PasswordField passwordField = new PasswordField();
        passwordField.setMaxWidth(200);

        javafx.scene.control.Button loginButton = new javafx.scene.control.Button("Login");
        loginButton.getStyleClass().add("buttons");

        loginButton.setOnAction(e -> handleLogin(usernameField.getText(), passwordField.getText()));

        javafx.scene.control.Hyperlink signupLink = new javafx.scene.control.Hyperlink("Sign Up");
        signupLink.getStyleClass().add("sign-up");

        signupLink.setOnAction(e -> openRegister());

        loginContent.setAlignment(Pos.CENTER);

        loginContent.getChildren().addAll(createLoginForm(usernameField, passwordField, loginButton, signupLink));

        // Remove the focus from the inputBox clicking anywhere.
        loginContent.setOnMouseClicked(e -> loginContent.requestFocus());
        // Remove the focus from the inputBox.
        Platform.runLater(loginContent::requestFocus);
        return loginContent;
    }
    private VBox createLoginForm(TextField usernameField, PasswordField passwordField, javafx.scene.control.Button loginButton, javafx.scene.control.Hyperlink signupLink) {

        VBox form = new VBox();
        form.getStyleClass().addAll("form", "form-container");

        form.setAlignment(Pos.CENTER);
        form.setSpacing(20);

        Region spacer1 = new Region();
        spacer1.setPrefHeight(10);
        form.getChildren().add(spacer1);

        Label titleLabel = new Label("LOG IN");
        titleLabel.getStyleClass().add("login-title");
        double maxWidth = 250;
        VBox usernameBox = createInputBox("Username", usernameField, maxWidth);
        VBox passwordBox = createInputBox("Password", passwordField, maxWidth);

        VBox.setMargin(signupLink, new Insets(0, 140, 0, 0));

        VBox buttonBox = new VBox();
        loginButton.setMaxWidth(maxWidth);
        buttonBox.getChildren().addAll(loginButton);
        buttonBox.setAlignment(Pos.CENTER);

        Region spacer = new Region();
        spacer.setPrefHeight(20);

        form.getChildren().addAll(titleLabel, usernameBox, passwordBox, signupLink, buttonBox, errorLabel, spacer);

        return form;
    }
    private VBox createInputBox(String labelText, javafx.scene.control.TextInputControl inputField, double maxWidth) {
        Label label = new Label(labelText);
        label.getStyleClass().addAll("input-label", "custom-label");

        Label additionalLabel = new Label("");
        additionalLabel.getStyleClass().add("error");

        VBox inputBox = new VBox();
        inputBox.getStyleClass().add("inputBox");

        HBox labelBox = new HBox();
        inputBox.getStyleClass().add("inputBox");
        Region spacingRegion = new Region();
        HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 79));
        HBox.setHgrow(additionalLabel, Priority.ALWAYS);
        labelBox.getChildren().addAll(label, spacingRegion, additionalLabel);

        additionalLabels.add(additionalLabel);

        VBox.setMargin(label, new Insets(0, 140, 0, 0));

        inputBox.setMaxWidth(maxWidth);
        inputField.setMaxWidth(maxWidth);
        inputBox.getChildren().addAll(labelBox, inputField);

        return inputBox;
    }
    private void handleLogin(String username, String password) {
        if(username.isEmpty()){
            additionalLabels.get(0).setText("Incorrect Username");
        }else{
            additionalLabels.get(0).setText("");
        }
        if(password.isEmpty()){
            additionalLabels.get(1).setText("Incorrect Password");
        }else{
            additionalLabels.get(1).setText("");
        }

        if (username.isEmpty() || password.isEmpty()){
            errorLabel.getStyleClass().add("error");
            errorLabel.setText("Please, insert username and password");
        } else {
            errorLabel.setText("");

            int accessCustomer = 0;

            CustomerMongoDBDAO cs = new CustomerMongoDBDAO();
            cs.openConnection();

            if(cs.authenticateCustomer(username , password) != null){
                // If the login has been successful.
                PollRedisDAO pollRedisDAO = new PollRedisDAO();
                String key = pollRedisDAO.getPollCookieOfUser(username); // Get the user cookie from Redis.
                System.out.println(key);
                if (key != null){
                    // The user already has a cookie in Redis.
                    customerInfo customer = convertJsonToObject(key, customerInfo.class);
                    assert customer != null;
                    //createUserCookie(customer); // Create a cookie file.
                    Session.setCustomerInfo(customer);
                } else {
                    // The user does not have a cookie in Redis.
                    customerInfo customer = new customerInfo(username, new ArrayList<>());
                    //createUserCookie(customer); // Create a cookie file.
                    Session.setCustomerInfo(customer);
                    pollRedisDAO.createPollCookieOfUser(username, customer.toString());

                }

                errorLabel.setText("");
                accessCustomer = 1;
            }else{
                AdminMongoDBDAO as = new AdminMongoDBDAO();
                as.openConnection();
                if(as.authenticateAdmin(username , password) != null) {
                    errorLabel.setText("");
                    accessCustomer = 2;
                }else{
                    errorLabel.getStyleClass().add("error");
                    errorLabel.setText("Login Failed");
                }
                as.closeConnection();
            }
            cs.closeConnection();
            //if the user is a registered user we go in the registered user home
            //otherwise in the admin one
            if(accessCustomer == 1) {

                HomeRegistered registered = new HomeRegistered(beansBetGUI);

                HBox registeredBar = registered.getHeader();
                beansBetGUI.getRoot().setTop(registeredBar);

                VBox homeContent = beansBetGUI.createHomeContent();
                beansBetGUI.getRoot().setCenter(homeContent);
            }else if(accessCustomer == 2){

                HomeAdmin admin = new HomeAdmin(beansBetGUI);

                HBox adminBar = admin.getHeader();
                beansBetGUI.getRoot().setTop(adminBar);

                VBox homeContent = beansBetGUI.createHomeContent();
                beansBetGUI.getRoot().setCenter(homeContent);
            }

        }
    }

    /**
     * This function open the registration page.
     */
    private void openRegister() {
        // The user is not registered, so we open the registration page.
        registerPage registerPage = new registerPage(beansBetGUI);
        beansBetGUI.getRoot().setTop(registerPage.getHeader());
        beansBetGUI.getRoot().setCenter(registerPage.getContent());
    }
}