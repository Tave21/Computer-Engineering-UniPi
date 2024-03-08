package it.unipi.dii;

import it.unipi.dii.pagesGUI.navBar;
import javafx.application.Application;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import java.io.FileNotFoundException;

import static it.unipi.dii.utility.MongoUtility.deactivateMongoDBNotifications;

public class BeansBetGUI extends Application {
    private BorderPane root;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) throws FileNotFoundException {

        deactivateMongoDBNotifications();
        primaryStage.setTitle("BeansBet");

        root = new BorderPane();

        navBar newHeader = new navBar(this);
        HBox header = new HBox();
        header = newHeader.createHeader(header, 0);
        root.setTop(header);
        root.getStylesheets().add(getClass().getResource("style.css").toExternalForm());

        root.setCenter(createHomeContent());
        Scene scene = new Scene(root, 800, 600);

        primaryStage.setScene(scene);

        primaryStage.show();
    }
    public VBox createHomeContent() {
        //creation of a VBox to contain the title and the image
        VBox homeContent = new VBox();
        homeContent.getStyleClass().add("welcome");
        homeContent.setAlignment(Pos.CENTER);

        //add an empty space as a node before the title
        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        //add centered title
        Label titleLabel = new Label("Welcome to BeansBet");
        titleLabel.getStyleClass().add("welcome-title");

        //creation of the image and its ImageView
        Image image = new Image(getClass().getResourceAsStream("/image/footballers.png"));
        ImageView imageView = new ImageView(image);

        //set the size of the image
        imageView.setFitWidth(1200);
        imageView.setFitHeight(500);

        //add everything to the content
        homeContent.getChildren().addAll(topSpacer, titleLabel, imageView);
        return homeContent;
    }
    public BorderPane getRoot() {
        return this.root;
    }
}