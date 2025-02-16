package it.unipi.dii.pagesGUI;

import it.unipi.dii.BeansBetGUI;
import it.unipi.dii.HomeRegistered;
import it.unipi.dii.dao.mongo.CustomerMongoDBDAO;
import it.unipi.dii.dao.redis.PollRedisDAO;
import it.unipi.dii.model.Customer;
import it.unipi.dii.model.customerInfo;
import javafx.scene.control.*;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.*;
import javafx.application.Platform;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.text.Text;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.dateTimes.getCurrentDateString;
import static it.unipi.dii.utility.dateTimes.isAdult;
import static it.unipi.dii.utility.securityLibrary.calculateSHA256;
import static it.unipi.dii.utility.regularExpressionChecks.*;

public class registerPage{
    private final BeansBetGUI beansBetGUI;
    private CheckBox femaleCheckBox;
    private CheckBox maleCheckBox;
    private final Label errorLabel;
    private final List<Label> additionalLabels = new ArrayList<>();

    public registerPage(BeansBetGUI beansBetGUI) {
        this.beansBetGUI = beansBetGUI;
        this.errorLabel = new Label("");
    }
    public HBox getHeader() {
        navBar buttonHeader = new navBar(beansBetGUI);
        HBox header = new HBox();
        header = buttonHeader.createHeader(header, 1);
        return header;
    }
    public VBox getContent() {

        VBox registerContent = new VBox();

        Region spacer = new Region();
        spacer.setPrefHeight(47);
        registerContent.getChildren().add(spacer);

        TextField nameField = new TextField();
        nameField.setMaxWidth(200);

        TextField surnameField = new TextField();
        surnameField.setMaxWidth(200);

        TextField emailField = new TextField();
        emailField.setMaxWidth(200);

        TextField cellNumberField = new TextField();
        cellNumberField.setMaxWidth(200);

        femaleCheckBox = createGenderCheckBox("Female");
        maleCheckBox = createGenderCheckBox("Male");

        DatePicker birthDate = new DatePicker();
        birthDate.setMaxWidth(200);

        TextField addressField = new TextField();
        addressField.setMaxWidth(200);

        TextField cityOfResidenceField = new TextField();
        cityOfResidenceField.setMaxWidth(200);

        TextField provinceField = new TextField();
        provinceField.setMaxWidth(200);

        TextField usernameField = new TextField();
        usernameField.setMaxWidth(200);

        // Get the value of the input fields for password and username.
        PasswordField passwordField = new PasswordField();
        passwordField.setMaxWidth(200);

        javafx.scene.control.Button registerButton = new javafx.scene.control.Button("Register");
        registerButton.getStyleClass().add("buttons");

        registerButton.setOnAction(e -> handleRegister(nameField.getText(), surnameField.getText(), emailField.getText(), cellNumberField.getText(), femaleCheckBox.isSelected(), maleCheckBox.isSelected(), birthDate.getValue(), addressField.getText(), cityOfResidenceField.getText(), provinceField.getText(), usernameField.getText(), passwordField.getText()));

        registerContent.setAlignment(Pos.CENTER);

        registerContent.getChildren().addAll(createRegisterForm(nameField, surnameField, emailField, cellNumberField, femaleCheckBox, maleCheckBox, birthDate,  addressField, cityOfResidenceField, provinceField, usernameField, passwordField, registerButton));

        ScrollPane scrollPane = new ScrollPane(registerContent);
        scrollPane.setFitToWidth(true);
        scrollPane.getStyleClass().add("matches_scroll");

        // Remove focus from inputBox clicking anywhere.
        registerContent.setOnMouseClicked(e -> registerContent.requestFocus());

        // Remove focus from inputBox.
        Platform.runLater(registerContent::requestFocus);

        return new VBox(scrollPane);
    }
    private CheckBox createGenderCheckBox(String labelText) {
        CheckBox checkBox = new CheckBox(labelText);
        checkBox.setMaxWidth(200);
        checkBox.getStyleClass().add("checkbox");
        checkBox.setGraphic(new Region());

        // Disables the entire clickable area of the text part of the CheckBox.
        checkBox.addEventFilter(MouseEvent.MOUSE_PRESSED, e -> {
            if (e.getTarget() instanceof Text || e.getTarget() == checkBox || e.getTarget() == checkBox.getGraphic()) {
                e.consume();
            }
        });
        // Handle the single selection manually.
        checkBox.setOnAction(event -> {
            if (checkBox.isSelected()) {
                if (checkBox.getText().equals("Female")) {
                    maleCheckBox.setSelected(false);
                } else {
                    femaleCheckBox.setSelected(false);
                }
            }
        });

        return checkBox;
    }
    private VBox createRegisterForm(TextField nameField, TextField surnameField, TextField emailField, TextField cellNumberField, CheckBox femaleCheckBox, CheckBox maleCheckBox, DatePicker birthDate, TextField addressField, TextField cityOfResidenceField, TextField provinceField, TextField usernameField, PasswordField passwordField, Button loginButton) {
        VBox form = new VBox();
        form.getStyleClass().addAll("form", "form-container");

        form.setAlignment(Pos.CENTER);
        form.setSpacing(20);

        Region spacer1 = new Region();
        spacer1.setPrefHeight(10);
        form.getChildren().add(spacer1);

        Label titleLabel = new Label("REGISTER");
        titleLabel.getStyleClass().add("login-title");
        double maxWidth = 300;

        VBox nameBox = createInputBox("Name", nameField, null, null, maxWidth);
        VBox surnameBox = createInputBox("Surname", surnameField, null, null, maxWidth);
        VBox emailBox = createInputBox("Email", emailField, null, null, maxWidth);
        VBox cellNumberBox = createInputBox("Cell Number", cellNumberField, null, null, maxWidth);

        VBox genderBox = new VBox();
        Region spacingRegion = new Region();
        VBox.setMargin(spacingRegion, new Insets(5, 0, 0, 0));
        genderBox.getChildren().addAll(femaleCheckBox, spacingRegion, maleCheckBox);
        genderBox = createInputBox("Gender", null, genderBox, null, maxWidth);

        VBox birthDateBox = createInputBox("Birthdate", null, null, birthDate, maxWidth);

        VBox addressBox = createInputBox("Address", addressField, null, null, maxWidth);
        VBox cityOfResidenceBox = createInputBox("City Of Residence", cityOfResidenceField, null, null, maxWidth);
        VBox provinceBox = createInputBox("Province", provinceField, null, null, maxWidth);
        VBox usernameBox = createInputBox("Username", usernameField, null, null, maxWidth);
        VBox passwordBox = createInputBox("Password", passwordField, null, null, maxWidth);

        VBox buttonBox = new VBox();
        loginButton.setMaxWidth(maxWidth);
        buttonBox.getChildren().addAll(loginButton);
        buttonBox.setAlignment(Pos.CENTER);

        Region spacer = new Region();
        spacer.setPrefHeight(20);

        form.getChildren().addAll(titleLabel, nameBox,  surnameBox, emailBox, cellNumberBox, genderBox, birthDateBox, addressBox, cityOfResidenceBox, provinceBox, usernameBox, passwordBox, buttonBox,  errorLabel, spacer);

        return form;
    }
    private VBox createInputBox(String labelText, javafx.scene.control.TextInputControl inputField, VBox inputVBox, DatePicker birthDateBox, double maxWidth) {
        Label label = new Label(labelText);
        label.getStyleClass().addAll("input-label", "custom-label");

        Label additionalLabel = new Label("");
        additionalLabel.getStyleClass().add("error");

        VBox inputBox = new VBox();
        inputBox.getStyleClass().add("inputBox");

        HBox labelBox = new HBox();
        inputBox.getStyleClass().add("inputBox");
        Region spacingRegion = new Region();
        HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 55));
        HBox.setHgrow(additionalLabel, Priority.ALWAYS);
        labelBox.getChildren().addAll(label, spacingRegion, additionalLabel);

        additionalLabels.add(additionalLabel);

        VBox.setMargin(label, new Insets(0, 140, 0, 0));

        inputBox.setMaxWidth(maxWidth);
        // If the inputVBox value is null, is the case of other input.
        // If the inputVBox value is not null is the checkbox.

        if(inputField != null) {
            inputField.setMaxWidth(maxWidth);
            inputBox.getChildren().addAll(labelBox, inputField);
        }else if(inputVBox != null){
            inputVBox.setMaxWidth(maxWidth);
            inputBox.getChildren().addAll(labelBox, inputVBox);
        }else{
            birthDateBox.setMaxWidth(maxWidth);
            inputBox.getChildren().addAll(labelBox, birthDateBox);
        }

        return inputBox;
    }
    private void handleRegister(String name, String surname, String email, String cellNumber, Boolean femaleCheckBox, Boolean maleCheckBox, LocalDate birthDate, String address, String city, String province, String username, String password) {
        int countErrors=0;
        if (name.isEmpty() || surname.isEmpty() || email.isEmpty() || cellNumber.isEmpty() ||
                (!femaleCheckBox && !maleCheckBox) || birthDate == null ||
                address.isEmpty() || city.isEmpty() || province.isEmpty() ||
                username.isEmpty() || password.isEmpty()) {
            errorLabel.getStyleClass().add("error");
            errorLabel.setText("Please, fill all the requested fields");
            countErrors++;
        } else {
            errorLabel.setText("");
        }

        if(isCorrectLength(name, 2 , 50) && isOnlyLetters(name , true)){
            additionalLabels.get(0).setText("");
        }else{
            additionalLabels.get(0).setText("Incorrect Name");
            countErrors++;
        }

        if(isCorrectLength(surname , 2 , 50) && isOnlyLetters(surname , true)){
            additionalLabels.get(1).setText("");
        }else{
            additionalLabels.get(1).setText("Incorrect Surname");
            countErrors++;
        }

        if(isCorrectLength(email , 5 , 100) && isValidEmail(email)){
            additionalLabels.get(2).setText("");
        }else{
            additionalLabels.get(2).setText("Incorrect Email");
            countErrors++;
        }

        if(isValidPhoneNumber(cellNumber)){
            additionalLabels.get(3).setText("");
        }else{
            additionalLabels.get(3).setText("Incorrect Cell Number");
            countErrors++;
        }

        if(birthDate == null){
            additionalLabels.get(5).setText("Incorrect Date");
            countErrors++;
        }else {
            if (isAdult(birthDate)) {
                additionalLabels.get(5).setText("");
            } else {
                additionalLabels.get(5).setText("Must be adult (18+) for register");
                countErrors++;
            }
        }

        if (isCorrectLength(address , 2 , 100) && isOnlyLettersAndNumbers(address , true)) {
            additionalLabels.get(6).setText("");
        } else {
            additionalLabels.get(6).setText("Invalid Address");
            countErrors++;
        }

        if(isCorrectLength(city, 1 , 50) ){
            additionalLabels.get(7).setText("");
        }else{
            additionalLabels.get(7).setText("Incorrect city name.");
            countErrors++;
        }

        if(isCorrectLength(province, 1 , 50) && isOnlyLetters(province , true)){
            additionalLabels.get(8).setText("");
        }else{
            additionalLabels.get(8).setText("Incorrect province name.");
            countErrors++;
        }

        if(isCorrectLength(username, 1 , 100) && isOnlyLettersAndNumbers(username , false)){
            additionalLabels.get(9).setText("");
        }else{
            additionalLabels.get(9).setText("Incorrect username.");
            countErrors++;
        }

        if(isCorrectLength(password, 3 , 200)){
            additionalLabels.get(10).setText("");
        }else{
            additionalLabels.get(10).setText("Incorrect Password");
            countErrors++;
        }

        if(countErrors == 0){
            // There aren't any error in the registration form.
            Customer cs = new Customer();
            cs.setName(name);
            if(femaleCheckBox){
                cs.setGender("F");
            }else{
                cs.setGender("M");
            }
            cs.setRegistrationDate(getCurrentDateString());
            cs.setSurname(surname);
            cs.setEmail(email);
            cs.setCellNumber(cellNumber);
            cs.setBirthDate(birthDate.toString());
            cs.setAddress(address);
            cs.setCityOfResidence(city);
            cs.setProvince(province);
            cs.setUsername(username);
            cs.setPassword(calculateSHA256(password)); // Compute the hash of the password.

            CustomerMongoDBDAO cDB = new CustomerMongoDBDAO();
            cDB.openConnection();
            final boolean b = cDB.registerCustomer(cs); // Insert the customer in MongoDB.
            cDB.closeConnection();

            if(b){
                customerInfo customer = new customerInfo(new ArrayList<>());
                Session.setUsername(username);
                Session.setCustomerInfo(customer);
                additionalLabels.get(9).setText(" ");
                openRegister();
            } else {
                additionalLabels.get(9).setText("This username already exists!");
            }
        }
    }
    private void openRegister() {
        HomeRegistered registered = new HomeRegistered(beansBetGUI);

        HBox registeredBar = registered.getHeader();
        beansBetGUI.getRoot().setTop(registeredBar);

        VBox homeContent = beansBetGUI.createHomeContent();
        beansBetGUI.getRoot().setCenter(homeContent);
    }
}