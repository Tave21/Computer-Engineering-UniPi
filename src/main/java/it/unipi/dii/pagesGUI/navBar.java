package it.unipi.dii.pagesGUI;

import it.unipi.dii.BeansBetGUI;
import it.unipi.dii.addElement;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;


public class navBar {

    private BeansBetGUI beansBetGUI;

    public navBar(BeansBetGUI beansBetGUI) {
        this.beansBetGUI = beansBetGUI;
    }

    public HBox createHeader(HBox header, int type) {
        //type indicates type of header
        //0 = unregistered header
        //1 = login/register header
        //2 = registered header
        //3 = admin header
        header.setPadding(new Insets(10));
        header.getStyleClass().add("header");
        Image logoImage = new Image(getClass().getResourceAsStream("/image/logo_BeansBet.png"));
        ImageView logoImageView = new ImageView(logoImage);
        logoImageView.setFitWidth(110);
        logoImageView.setPreserveRatio(true);

        HBox menu = createMenu(type);
        //set the Insets to achieve the desired space between elements
        HBox.setMargin(logoImageView, new Insets(0, 20, 0, 0));

        if(type == 0 || type == 2 || type == 3) {
            // Login and Register buttons
            HBox rightButtons = createRightButtons(type);
            HBox.setMargin(menu, new Insets(0, 20, 0, 0));
            //add an empty Region to push the rightButtons to the right
            Region spacer = new Region();
            HBox.setHgrow(spacer, Priority.ALWAYS);
            header.getChildren().addAll(logoImageView, menu, spacer, rightButtons);

        }else if (type == 1) {

            header.getChildren().addAll(logoImageView, menu);
        }
        header.setAlignment(Pos.CENTER_LEFT);

        return header;
    }
    private HBox createMenu(int type) {
        //25 is the distance between all the labels inside the HBox
        HBox menu = new HBox(25);
        menu.setAlignment(Pos.CENTER);
        Label homeLabel = createMenuLabel("", "/image/home1.png", type);
       if(type == 0){
           Label liveLabel = createMenuLabel("Live", "", type);
           Label matchesLabel = createMenuLabel("Matches", "", type);
           Label acitivePollsLabel = createMenuLabel("Active Polls", "", type);
           Label terminatedPollsLabel = createMenuLabel("Terminated Polls", "", type);
           menu.getChildren().addAll(homeLabel, liveLabel, matchesLabel, acitivePollsLabel, terminatedPollsLabel);
       }else if(type == 1){
           Label homeLabel2 = createMenuLabel("", "/image/home.png", type);
           menu.getChildren().addAll(homeLabel2);
       }else if(type == 2) {
           Label liveLabel = createMenuLabel("Live", "", type);
           Label matchesLabel = createMenuLabel("Matches", "", type);
           Label acitivePollsLabel = createMenuLabel("Active Polls", "", type);
           Label terminatedPollsLabel = createMenuLabel("Terminated Polls", "", type);
           Label slipsCartLabel = createMenuLabel("Slips' Cart", "", type);
           Label ConfirmedSlipsLabel = createMenuLabel("Confirmed Slips", "", type);
           menu.getChildren().addAll(homeLabel, liveLabel, matchesLabel, acitivePollsLabel, terminatedPollsLabel, slipsCartLabel, ConfirmedSlipsLabel);
        }else if(type == 3){
            Label addLabel = createMenuLabel("Add Element", "", type);
            Label statsLabel = createMenuLabel("Stats", "", type);
            Label customersLabel = createMenuLabel("Customers", "", type);
            menu.getChildren().addAll(homeLabel, addLabel, statsLabel, customersLabel);
        }

        return menu;
    }
    private Label createMenuLabel(String text, String imagePath, int type) {
        Label label = new Label(text);
        label.getStyleClass().add("menu-label");

        //if the image is specified (string doesn't be ""), set icon, otherwise return label
        if (!imagePath.isEmpty()) {
            //assign the image to the label
            assignImageToLabel(label, imagePath);
            //handle the style when mouse is over a label
            label.setOnMouseEntered(event -> handleMouse(label, true));
            //handle the style when the mouse goes out to the label
            label.setOnMouseExited(event -> handleMouse(label, false));
        }
        //handle of the navigation between pages
        label.setOnMouseClicked(event -> handleNavigation(label, text, type));

        return label;
    }
    private void assignImageToLabel(Label label, String imagePath){

        Image icon = new Image(getClass().getResourceAsStream(imagePath));
        ImageView iconImageView = new ImageView(icon);
        iconImageView.setFitWidth(20);
        iconImageView.setFitHeight(20);
        label.setGraphic(iconImageView);
    }
    //handle the mouse on the home icon when mouse is over the icon
    //handle both the cases, when the mouse is IN and when is OUT the icon
    private void handleMouse(Label selectedLabel, boolean in_out){
        boolean isFirstLabel = true;
        HBox parentHBox = (HBox) selectedLabel.getParent();
        int numberOfLabels = parentHBox.getChildren().size();

        if(in_out) {
            assignImageToLabel(selectedLabel, "/image/home1.png");
        }

        for (javafx.scene.Node node : ((HBox) selectedLabel.getParent()).getChildren()) {
            if (node instanceof Label) {
                Label currentLabel = (Label) node;  //the current label in the for loop
                //we recognize the first label (home)
                if (isFirstLabel) {
                    //if the selected label by the user is not the first (home), the home logo must be white
                    if(currentLabel.equals(selectedLabel)) {
                        //if there is only one label, we have to switch only the home label
                        //without these, the checkColor function won't recognize if the home
                        //label is green or white, it checks the text colors of the other labels
                        if(checkColor(selectedLabel) || numberOfLabels == 1) {
                            //if in_out is true, handle the mouse over event
                            //otherwise handle the mouse out event

                           if(in_out) {
                                assignImageToLabel(currentLabel, "/image/home1.png");
                            }else{
                                assignImageToLabel(currentLabel, "/image/home.png");
                            }
                        }
                    }
                    isFirstLabel = false;   //we are not in the first lable anymore
                }
            }
        }
    }
    //check the colors of all the labels in the menu, if one is green, return true,
    //if returns true, the icon of the home is white and must become green,
    //otherwise the icon is green and must be green, in the cae of mouse IN.
    //In the case of mouse OUT, if the icon of the home is white, must return white,
    //if the icon is green, must be green.
    private boolean checkColor(Label selectedLabel){
        boolean isFirstLabel = true;
        boolean isGreen =false;
        Color textColor;
        double red;
        double green;
        double blue;

        for (javafx.scene.Node node : ((HBox) selectedLabel.getParent()).getChildren()){
            if (node instanceof Label) {
                Label currentLabel = (Label) node;
                //we need this because, otherwise the first element is counted as green and when the
                //mouse goes out, the icon will become white instead of remaining green
                if(!isFirstLabel) {
                    textColor = (Color) currentLabel.getTextFill();
                    red = textColor.getRed();
                    green = textColor.getGreen();
                    blue = textColor.getBlue();
                    if (red == 1.0 && green == 1.0 && blue == 1.0) {
                        //color white
                    } else {
                        isGreen = true;
                        break;
                    }
                }
                isFirstLabel = false;
            }
        }
        return isGreen;
    }
    //handle the navigation between all the labels in the menu
    private void handleNavigation(Label selectedLabel, String section, int type) {

        boolean isFirstLabel = true;

        for (javafx.scene.Node node : ((HBox) selectedLabel.getParent()).getChildren()) {
            if (node instanceof Label) {
                Label currentLabel = (Label) node;  //the current label in the for loop
                //we recognize the first label (home)
                if (isFirstLabel) {
                    //if the selected label by the user is not the first (home), the home logo must be white
                    if (!currentLabel.equals(selectedLabel)) {
                        assignImageToLabel(currentLabel, "/image/home.png");
                    } else {
                        //if the selected label by the user is the first (home), the home logo must be green
                        //System.out.println("entra?");
                        assignImageToLabel(currentLabel, "/image/home1.png");
                    }
                    isFirstLabel = false;   //we are not in the first lable anymore
                }
            }
        }
        for (javafx.scene.Node node : ((HBox) selectedLabel.getParent()).getChildren()) {
            if (node instanceof Label) {
                //necessary, otherwise, the label is still green
                ((Label) node).getStyleClass().remove("selected-label");
                ((Label) node).getStyleClass().add("menu-label");
            }
        }
        //we add the class selected-label,to the selected one by the user
        selectedLabel.getStyleClass().add("selected-label");

        navigate(section, type);
    }
    private void navigate(String section, int type){
        if(type == 0){
            switch (section) {
                case "":
                    VBox homeContent = beansBetGUI.createHomeContent();
                    beansBetGUI.getRoot().setCenter(homeContent);
                    break;
                case "Live":
                    livePage livePage = new livePage();
                    beansBetGUI.getRoot().setCenter(livePage.getContent(false));
                    break;
                case "Matches":
                    matchesPage matchesPage = new matchesPage();
                    beansBetGUI.getRoot().setCenter(matchesPage.getContent(false));
                    break;
                case "Active Polls":
                    pollsPage activePollsPage = new pollsPage();
                    beansBetGUI.getRoot().setCenter(activePollsPage.getContent(false, true));
                    break;
                case "Terminated Polls":
                    pollsPage terminatedPollsPage = new pollsPage();
                    beansBetGUI.getRoot().setCenter(terminatedPollsPage.getContent(false, false));
                    break;
            }
        }else if(type == 1){

            HBox header = new HBox();
            HBox originalHeader = createHeader(header, 0);
            VBox homeContent = beansBetGUI.createHomeContent();

            //set the navigation bar and content in the BorderPane of BeansBetGUI
            beansBetGUI.getRoot().setTop(originalHeader);
            beansBetGUI.getRoot().setCenter(homeContent);
        }else if(type == 2){
            switch (section) {
                case "":
                    VBox homeContent = beansBetGUI.createHomeContent();
                    beansBetGUI.getRoot().setCenter(homeContent);
                    break;
                case "Live":
                    livePage livePage = new livePage();
                    beansBetGUI.getRoot().setCenter(livePage.getContent(false));
                    break;
                case "Matches":
                    matchesPage matchesPage = new matchesPage();
                    beansBetGUI.getRoot().setCenter(matchesPage.getContent(true));
                    break;
                case "Active Polls":
                    pollsPage activePollsPage = new pollsPage();
                    beansBetGUI.getRoot().setCenter(activePollsPage.getContent(true, true));
                    break;
                case "Terminated Polls":
                    pollsPage terminatedPollsPage = new pollsPage();
                    beansBetGUI.getRoot().setCenter(terminatedPollsPage.getContent(true, false));
                    break;
                case "Slips' Cart":
                    slipsCartPage slipsCartPage = new slipsCartPage();
                    beansBetGUI.getRoot().setCenter(slipsCartPage.getContent());
                    break;
                case "Confirmed Slips":
                    confirmedSlipsPage confirmedSlipsPage = new confirmedSlipsPage();
                    beansBetGUI.getRoot().setCenter(confirmedSlipsPage.getContent());
                    break;
            }
        }else if(type == 3){
            switch (section) {
                case "":
                    VBox homeContent = beansBetGUI.createHomeContent();
                    beansBetGUI.getRoot().setCenter(homeContent);
                    break;
                case "Add Element":
                    addElement addPage = new addElement();
                    beansBetGUI.getRoot().setCenter(addPage.getContent());
                    break;
                case "Stats":
                    statsPage statsPage = new statsPage();
                    beansBetGUI.getRoot().setCenter(statsPage.getContent());
                    break;
                case "Customers":
                    customerPage customersPage = new customerPage();
                    beansBetGUI.getRoot().setCenter(customersPage.getContent());
                    break;
            }
        }
    }
    private HBox createRightButtons(int type) {
        HBox rightButtons = new HBox(10);
        rightButtons.setAlignment(Pos.CENTER);

       if(type == 2 || type == 3) {
           Button logoutButton = createButton("LOG OUT");
           logoutButton.setOnMouseClicked(event -> handleLogout());

           rightButtons.getChildren().add(logoutButton);
       }else if(type == 0) {
           //box globale di entrambi i bottoni
           Button loginButton = createButton("LOG IN");
           loginButton.setOnMouseClicked(event -> handleLogin());
           Button registerButton = createButton("REGISTER");
           registerButton.setOnMouseClicked(event -> handleRegister());

           rightButtons.getChildren().addAll(loginButton, registerButton);
       }

        return rightButtons;
    }
    private Button createButton(String text) {
        Button button = new Button(text);
        button.getStyleClass().add("right-buttons");
        return button;
    }
    private void handleLogout() {

        HBox header = new HBox();
        HBox originalHeader = createHeader(header, 0);
        VBox homeContent = beansBetGUI.createHomeContent();

        beansBetGUI.getRoot().setTop(originalHeader);
        beansBetGUI.getRoot().setCenter(homeContent);
    }
    private void handleLogin() {

        loginPage loginPage = new loginPage(beansBetGUI);

        HBox loginBar = loginPage.getHeader();
        //get login page content
        VBox loginContent = loginPage.getContent();

        beansBetGUI.getRoot().setTop(loginBar);

        beansBetGUI.getRoot().setCenter(loginContent);
    }
    private void handleRegister() {

        registerPage registerPage = new registerPage(beansBetGUI);
        HBox registerBar = registerPage.getHeader();

        //get login page register content
        VBox registerContent = registerPage.getContent();
        beansBetGUI.getRoot().setTop(registerBar);

        beansBetGUI.getRoot().setCenter(registerContent);
    }
}