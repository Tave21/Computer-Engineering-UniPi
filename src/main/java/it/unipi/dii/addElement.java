package it.unipi.dii;

import it.unipi.dii.dao.redis.PollRedisDAO;
import it.unipi.dii.dao.mongo.MatchMongoDBDAO;
import it.unipi.dii.model.Match;
import it.unipi.dii.model.Poll;
import it.unipi.dii.model.pollOption;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.layout.*;

import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static it.unipi.dii.utility.DateTimes.checkTimestampFormat;

public class addElement {
    private List<Label> additionalLabels = new ArrayList<>();
    public StackPane getContent(){
        /*//TEST FOR LIVE MATCH CONSISTENCY
        List<Match> ml = new ArrayList<>();
        MatchMongoDBDAO c = new MatchMongoDBDAO();
        c.openConnection();
        Match match = new Match();
        match.setMatchID(10165); //change matchID
        match.setTeam_home("Fulham FC"); //change team names and data for testing in another match
        match.setTeam_away("Tottenham Hotspur FC");
        match.setAway_goals(0);
        match.setHome_goals(0);
        match.setMatchDate("2024-03-16T17:30:00Z");
        match.setCompetition_id("GB1");
        match.setStatus("IN_PLAY");
        match.setMultiplier(0, "1", 1.2);
        match.setMultiplier(1, "1", 1.2);
        match.setMultiplier(2, "1", 1.2);
        match.setMultiplier(3, "1", 1.2);
        match.setMultiplier(4, "1", 1.2);
        match.setMultiplier(5, "1", 1.2);
        match.setMultiplier(6, "1", 1.2);
        match.setMultiplier(7, "1", 1.2);
        match.setMultiplier(8, "1", 1.2);
        match.setMultiplier(9, "1", 1.2);
        match.setMultiplier(10, "1", 1.2);
        match.setMultiplier(11, "1", 1.2);
        match.setMultiplier(12, "1", 1.2);
        match.setMultiplier(13, "1", 1.2);
        match.setMultiplier(14, "1", 1.2);
        match.setMultiplier(15, "1", 1.2);
        match.setMultiplier(16, "1", 1.2);
        match.setMultiplier(17, "1", 1.2);
        match.setMultiplier(18, "1", 1.2);
        match.setMultiplier(19, "1", 1.2);
        match.setMultiplier(20, "1", 1.2);
        match.setMultiplier(21, "1", 1.2);
        match.setMultiplier(22, "1", 1.2);
        ml.add(match);
        try {
            c.updateMatches(ml);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        */

        //main content of the addElemt page
        StackPane stackPane = new StackPane();

        //white space before the title
        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        //new white space
        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(50);

        //add title
        Label titleLabel = new Label("Add New Elements");
        titleLabel.getStyleClass().add("live-title");

        VBox survey = createNewElement();
        survey.setMaxWidth(350);

        HBox matchesContainer = new HBox();
        matchesContainer.setAlignment(Pos.CENTER);
        matchesContainer.setSpacing(20);
        matchesContainer.setPrefWidth(700);
        matchesContainer.setPrefHeight(Region.USE_COMPUTED_SIZE);

        matchesContainer.getChildren().addAll(survey);

        HBox.setHgrow(survey, Priority.ALWAYS);

        //add everything to the content
        VBox liveContent = new VBox();
        liveContent.setMaxWidth(700);
        liveContent.setAlignment(Pos.CENTER);
        liveContent.setSpacing(20);
        liveContent.getChildren().addAll(topSpacer, titleLabel, matchesContainer, bottomSpacer);

        //add content to the StackPane
        stackPane.getChildren().addAll(liveContent);
        //add StackPane to the ScrollPane
        ScrollPane scrollPane = new ScrollPane(stackPane);
        scrollPane.setFitToWidth(true); //fit the ScrollPane to the width
        scrollPane.getStyleClass().add("matches_scroll");

        //center alignment for the ScrollPane
        VBox.setVgrow(scrollPane, Priority.ALWAYS);

        return new StackPane(scrollPane);
    }
    protected VBox createNewElement() {

        VBox form = new VBox();
        form.getStyleClass().addAll("form", "form-container");
        //add center alignment and inside spacing
        form.setAlignment(Pos.CENTER);
        form.setSpacing(5);

        //add match
        Region titleSpacer = new Region();
        titleSpacer.setPrefHeight(20);

        Label titleLabel = new Label("Add new poll");
        titleLabel.getStyleClass().addAll("input-label");

        Region topSpacer = new Region();
        topSpacer.setPrefHeight(40);

        Label errorQuestionLabel1 = new Label("");
        Label errorQuestionLabel2 = new Label("");
        errorQuestionLabel2.getStyleClass().addAll("error");
        HBox questionBox1 = new HBox();

        Region spacingRegion = new Region();
        HBox.setHgrow(spacingRegion, Priority.ALWAYS);  //space to insert the second label on the right
        HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 55));

        questionBox1.getChildren().addAll(errorQuestionLabel1, spacingRegion, errorQuestionLabel2);
        additionalLabels.add(errorQuestionLabel2);

        Label questionLabel = new Label("Question");
        questionLabel.getStyleClass().addAll("input-label");
        TextField questionField = new TextField();
        HBox questionBox = new HBox();

        Region spacingRegion1 = new Region();
        HBox.setHgrow(spacingRegion1, Priority.ALWAYS);
        HBox.setMargin(spacingRegion1, new Insets(0, 0, 0, 55));

        questionBox.getChildren().addAll(questionLabel, spacingRegion1, questionField);


        Label errorType= new Label("");
        Label errorType2= new Label("");
        errorType2.getStyleClass().addAll("error");
        HBox errorBox1 = new HBox();

        Region Typeregion= new Region();
        HBox.setHgrow(Typeregion, Priority.ALWAYS);  // seconda label a destra
        HBox.setMargin(Typeregion, new Insets(0, 0, 0, 55));

        errorBox1.getChildren().addAll(errorType, Typeregion, errorType2);
        additionalLabels.add(errorType2);

        Label type= new Label("Type");
        type.getStyleClass().addAll("input-label");
        TextField TypeField = new TextField();
        HBox qBox = new HBox();

        Region Typeregion1 = new Region();
        HBox.setHgrow(Typeregion1, Priority.ALWAYS);  // seconda label a destra
        HBox.setMargin(Typeregion1, new Insets(0, 0, 0, 55));

        qBox.getChildren().addAll(type, Typeregion1, TypeField);


        Label errorDate= new Label("");
        Label errorDate2= new Label("");
        errorDate2.getStyleClass().addAll("error");
        HBox errorBox2 = new HBox();

        Region Dateregion= new Region();
        HBox.setHgrow(Dateregion, Priority.ALWAYS);
        HBox.setMargin(Dateregion, new Insets(0, 0, 0, 55));

        errorBox2.getChildren().addAll(errorDate, Dateregion, errorDate2);
        additionalLabels.add(errorDate2);

        Label Date= new Label("Activation Date");
        Date.getStyleClass().addAll("input-label");
        TextField DateField = new TextField();
        HBox qBox1 = new HBox();

        Region DateRegion1= new Region();
        HBox.setHgrow(DateRegion1, Priority.ALWAYS);
        HBox.setMargin(DateRegion1, new Insets(0, 0, 0, 55));

        qBox1.getChildren().addAll(Date, DateRegion1, DateField);


        Label errorOption1Label1 = new Label("");
        Label errorOption1Label2 = new Label("");
        errorOption1Label2.getStyleClass().addAll("error");
        HBox option1Box1 = new HBox();

        Region spacingRegion2 = new Region();
        HBox.setHgrow(spacingRegion2, Priority.ALWAYS);
        HBox.setMargin(spacingRegion2, new Insets(0, 0, 0, 55));

        option1Box1.getChildren().addAll(errorOption1Label1, spacingRegion2, errorOption1Label2);
        additionalLabels.add(errorOption1Label2);

        Label option1Label = new Label("Option 1");
        option1Label.getStyleClass().addAll("input-label");
        TextField option1Field = new TextField();
        HBox option1Box = new HBox();

        Region spacingRegion3 = new Region();
        HBox.setHgrow(spacingRegion3, Priority.ALWAYS);
        HBox.setMargin(spacingRegion3, new Insets(0, 0, 0, 55));

        option1Box.getChildren().addAll(option1Label, spacingRegion3, option1Field);


        Label errorOption2Label1 = new Label("");
        Label errorOption2Label2 = new Label("");
        errorOption2Label2.getStyleClass().addAll("error");
        HBox option2Box1 = new HBox();

        Region spacingRegion4 = new Region();
        HBox.setHgrow(spacingRegion4, Priority.ALWAYS);
        HBox.setMargin(spacingRegion4, new Insets(0, 0, 0, 55));

        option2Box1.getChildren().addAll(errorOption2Label1, spacingRegion4, errorOption2Label2);
        additionalLabels.add(errorOption2Label2);

        Label option2Label = new Label("Option 2");
        option2Label.getStyleClass().addAll("input-label");
        TextField option2Field = new TextField();
        HBox option2Box = new HBox();

        Region spacingRegion5 = new Region();
        HBox.setHgrow(spacingRegion5, Priority.ALWAYS);
        HBox.setMargin(spacingRegion5, new Insets(0, 0, 0, 55));
        option2Box.getChildren().addAll(option2Label, spacingRegion5, option2Field);


        Label option3Label = new Label("Option 3");
        option3Label.getStyleClass().addAll("input-label");
        TextField option3Field = new TextField();
        HBox option3Box = new HBox();

        Region spacingRegion7 = new Region();
        HBox.setHgrow(spacingRegion7, Priority.ALWAYS);
        HBox.setMargin(spacingRegion7, new Insets(0, 0, 0, 55));
        option3Box.getChildren().addAll(option3Label, spacingRegion7, option3Field);

        Label option4Label = new Label("Option 4");
        option4Label.getStyleClass().addAll("input-label");
        TextField option4Field = new TextField();
        HBox option4Box = new HBox();

        Region spacingRegion8 = new Region();
        HBox.setHgrow(spacingRegion8, Priority.ALWAYS);
        HBox.setMargin(spacingRegion8, new Insets(0, 0, 0, 55));
        option4Box.getChildren().addAll(option4Label, spacingRegion8, option4Field);

        Label option5Label = new Label("Option 5");
        option5Label.getStyleClass().addAll("input-label");
        TextField option5Field = new TextField();
        HBox option5Box = new HBox();

        Region spacingRegion9= new Region();
        HBox.setHgrow(spacingRegion9, Priority.ALWAYS);
        HBox.setMargin(spacingRegion9, new Insets(0, 0, 0, 55));
        option5Box.getChildren().addAll(option5Label, spacingRegion9, option5Field);

        Insets hboxMargins = new Insets(5, 5, 5, 5);

        VBox.setMargin(questionBox, hboxMargins);
        VBox.setMargin(qBox, hboxMargins);
        VBox.setMargin(qBox1, hboxMargins);
        VBox.setMargin(option1Box, hboxMargins);
        VBox.setMargin(option2Box, hboxMargins);
        VBox.setMargin(option3Box, hboxMargins);
        VBox.setMargin(option4Box, hboxMargins);
        VBox.setMargin(option5Box, hboxMargins);
        VBox.setMargin(questionBox1, hboxMargins);
        VBox.setMargin(errorBox1, hboxMargins);
        VBox.setMargin(errorBox2, hboxMargins);
        VBox.setMargin(option1Box1, hboxMargins);
        VBox.setMargin(option2Box1, hboxMargins);

        Label buttonLabel =new Label("");
        buttonLabel.getStyleClass().addAll("error");
        additionalLabels.add(buttonLabel);

        Region buttonSpacer = new Region();
        buttonSpacer.setPrefHeight(70);

        Button addButton = new Button("Add");
        addButton.getStyleClass().add("right-buttons");

        addButton.setOnAction(e -> handleSurvey(questionField.getText(),TypeField.getText(), DateField.getText(), option1Field.getText(), option2Field.getText(), option3Field.getText(), option4Field.getText(), option5Field.getText()));

        form.setPrefWidth(700);
        form.getChildren().addAll(titleSpacer, titleLabel, questionBox1, questionBox, errorBox1,qBox, errorBox2,qBox1, option1Box1, option1Box, option2Box1, option2Box, option3Box, option4Box, option5Box, /*buttonSpacer,*/ buttonLabel, addButton);

        Region bottomSpacer = new Region();
        VBox.setVgrow(bottomSpacer, Priority.ALWAYS);
        form.getChildren().add(bottomSpacer);

        return form;
    }
    private void handleSurvey(String question,String type, String date, String option1, String option2, String option3 , String option4 , String option5){
        int countErrors=0;
        //check only in the firt two options
        if (question.isEmpty() || option1.isEmpty() || option2.isEmpty() || type.isEmpty() || date.isEmpty()) {
            additionalLabels.get(5).setText("Please, fill all the mandatory fields");
            countErrors++;
        } else {

            additionalLabels.get(5).setText("");
        }

        String regex = "^[a-zA-Z0-9\\s]*\\?$";

        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(question);

        if(matcher.matches()){
            additionalLabels.get(0).setText("");
        }else{
            additionalLabels.get(0).setText("Please enter a correct question");
            countErrors++;
        }

        String regex2 = "^[a-zA-Z]+$";
        Pattern pattern2 = Pattern.compile(regex2);
        Matcher matcher2 = pattern2.matcher(type);

        if(matcher2.matches()){
            additionalLabels.get(1).setText("");
        }else{
            additionalLabels.get(1).setText("Please enter correct type");
            countErrors++;
        }

        if(checkTimestampFormat(date)){
            additionalLabels.get(2).setText("");
        }else{
            additionalLabels.get(2).setText("Please enter correct date:\n 2012-08-25T21:00:00Z");
            countErrors++;
        }


        String regex1 = "^[a-zA-Z0-9]+$";
        if(Pattern.matches(regex1, option1)){
            additionalLabels.get(3).setText("");
            System.out.println("entra team1");
        }else{
            additionalLabels.get(3).setText("Please enter a correct option");
            countErrors++;
        }
        if(Pattern.matches(regex1, option2)){
            additionalLabels.get(4).setText("");
            System.out.println("entra team2");
        }else{
            additionalLabels.get(4).setText("Please enter a correct option");
            countErrors++;
        }

        //if all mandatory the fields are filled and correct, we add the element
        if(countErrors == 0){
            //add element
            List<String> options = new ArrayList<>();

            options.add(option1);
            options.add(option2);

            if (!option3.isEmpty()){
                options.add(option3);
            }
            if (!option4.isEmpty()){
                options.add(option4);
            }
            if (!option5.isEmpty()){
                options.add(option5);
            }
            List<pollOption> pollOptions = new ArrayList<>();
            for(int i = 0; i < options.size(); i++){
                pollOption option = new pollOption();
                option.setOptionCaption(options.get(i));
                option.setOptionVotes(0);
                pollOptions.add(option);
            }
            Poll poll = new Poll();
            poll.setPollName(question);
            poll.setOptions(pollOptions);
            poll.setCreationDate(Instant.now().truncatedTo(ChronoUnit.SECONDS).toString());
            poll.setNumberOfVotes(0);
            poll.setPollType(type);
            poll.setActivationDate(date);

            PollRedisDAO pollRedisDAO = new PollRedisDAO();
            List<Poll> list = new ArrayList<>();
            list = pollRedisDAO.getAllPollFromRedis();
            int i = 1; //prendo il primo indice disponibile
            for (Poll s : list) {
                if(i != s.getPollID()){
                    break;
                }
                i++;
            }
            poll.setPollID(i);

            pollRedisDAO.addPollToRedis(poll);
        }
    }
}
