package it.unipi.dii.pagesGUI;
import com.mongodb.client.AggregateIterable;
import it.unipi.dii.dao.mongo.PollMongoDBDAO;
import it.unipi.dii.dao.redis.PollRedisDAO;
import it.unipi.dii.model.Poll;
import it.unipi.dii.model.pollOption;
import it.unipi.dii.userCookie.customerVotedPollVoice;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.*;

import javafx.scene.layout.*;
import org.bson.Document;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.userCookie.usernameCookie.createUserCookie;
import static it.unipi.dii.utility.DateTimes.getCurrentInstant;
import static it.unipi.dii.utility.JsonToDocument.convertDocumentToJson;
import static it.unipi.dii.utility.JsonToObjectConverter.convertJsonToObject;

public class pollsPage {
    private List<pollProgressBar> progressList = new ArrayList<>();

    public StackPane getContent(boolean registered, boolean active) {

        StackPane stackPane = new StackPane();

        Region topSpacer = new Region();
        topSpacer.setPrefHeight(20);

        Region bottomSpacer = new Region();
        bottomSpacer.setPrefHeight(50);

        Label titleLabel = new Label("");
        titleLabel.getStyleClass().add("live-title");
        if(active){
            titleLabel.setText("Active Polls");
        }else{
            titleLabel.setText("Terminated Polls");
        }

        VBox pollsContent = new VBox();
        pollsContent.setMaxWidth(350);
        pollsContent.setAlignment(Pos.CENTER);
        pollsContent.setSpacing(20);

        if(!active) {
            PollMongoDBDAO pMongo = new PollMongoDBDAO();
            pMongo.openConnection();

            List<Document> pipeline = Arrays.asList(new Document("$sort",
                            new Document("activationDate", -1L)),
                    new Document("$project",
                            new Document("_id", 0L)));
            AggregateIterable<Document> docs = pMongo.mongoDB.getCollection("polls").aggregate(pipeline);

            VBox choice;
            pollsContent.getChildren().addAll(topSpacer, titleLabel);

            for (Document document : docs) {
                System.out.println(convertDocumentToJson(document));
                Poll p = convertJsonToObject(convertDocumentToJson(document), Poll.class);
                if (p != null) {
                    choice = createResult(registered, active, p.getPollName(), p.getOptions(),p.getPollID(), null);
                    choice.setMaxWidth(350);

                    pollsContent.getChildren().addAll(choice);
                }
            }
            pMongo.closeConnection();
            pollsContent.getChildren().addAll(bottomSpacer);
        }else{
            // Redis management

            PollRedisDAO pRedis = new PollRedisDAO();
            List<Poll> polllist = pRedis.getAllPollFromRedis();
            //check date of poll
            for (int i = 0; i < polllist.size(); i++) {
                int id = polllist.get(i).getPollID();
                //String caption = polllist.get(i).getPollName();
                //System.out.println("id = "+ id);
                Instant now = getCurrentInstant();
                Instant activationDate = Instant.parse(polllist.get(i).getActivationDate());
                Instant activationDatePlusOneDay = activationDate.plus(Duration.ofDays(1));
                //System.out.println("question: "+ caption);
                //System.out.println("now: "+ now);
                //System.out.println("activation date: "+ activationDate);
                //System.out.println("activation date plus one: "+ activationDatePlusOneDay);
                if (now.isBefore(activationDate)){ //remove poll if it is not active yet
                    polllist.remove(i);
                    //we remove an element of the list, the next element will replace it, so
                    //we have to decrement the current index, otherwise we'll skip an element
                    i--;
                }
                if(activationDatePlusOneDay.isBefore(now)){ //remove poll if it is expired, one day after activation
                    //System.out.println("expired");
                    PollRedisDAO pRedis2 = new PollRedisDAO();
                    pRedis2.addPollToMongoDB(polllist.get(i)); // Send the poll to MongoDB.
                    // Remove the poll from redis if it is elapsed.
                    pRedis2.removePollfromRedis(id);
                    polllist.remove(i);
                    //we remove an element of the list, the next element will replace it, so
                    //we have to decrement the current index, otherwise we'll skip an element
                    i--;
                }
            }

            pollsContent.getChildren().addAll(topSpacer, titleLabel);
            for (Poll p : polllist) {
                String votedCaption;
                if(registered){
                    try {
                        votedCaption = Session.getCustomerInfo().OptionPresent(p.getPollID());
                    }catch(NullPointerException e){
                        votedCaption = null;
                    }
                }else{
                    votedCaption = null;
                }

                VBox choice = createResult(registered, active, p.getPollName(), p.getOptions(), p.getPollID(), votedCaption);
                choice.setMaxWidth(350);
                pollsContent.getChildren().addAll(choice);
            }
            pollsContent.getChildren().add(bottomSpacer);
        }

        stackPane.getChildren().addAll(pollsContent);
        ScrollPane scrollPane = new ScrollPane(stackPane);
        scrollPane.setFitToWidth(true);
        scrollPane.getStyleClass().add("matches_scroll");

        VBox.setVgrow(scrollPane, Priority.ALWAYS);

        return new StackPane(scrollPane);
    }

    protected VBox createResult(boolean registered, boolean active, String question, List<pollOption> options, Integer pollID, String votedCaption) {

        VBox form = new VBox();
        form.getStyleClass().addAll("form", "form-container");

        form.setAlignment(Pos.CENTER);
        form.setSpacing(5);

        Label questionLabel = new Label(question);
        questionLabel.getStyleClass().add("input-label");

        ToggleGroup optionGroup = new ToggleGroup();

        double sum = 0;

        List<Double> Perc = new ArrayList<>();
        List<RadioButton> RadioButtons = new ArrayList<>();

        for (it.unipi.dii.model.pollOption pollOption : options) {
            RadioButton radioOption1 = new RadioButton(pollOption.getOptionCaption());
            //if the user has already voted that caption, he will see the relative button selected
            if(votedCaption != null && votedCaption.equals(pollOption.getOptionCaption())){
                radioOption1.setSelected(true);
            }
            radioOption1.setToggleGroup(optionGroup);
            radioOption1.getStyleClass().add("radio-button");

            RadioButtons.add(radioOption1);

            sum = sum + pollOption.getOptionVotes();

        }

        for(pollOption option : options){
            if(sum != 0){
                Perc.add(option.getOptionVotes()/sum);
            }else{
                Perc.add(0.0);
            }
        }

        List<HBox> HBoxes = new ArrayList<>();

        for(int i = 0 ; i < options.size() ; i++) {
            HBoxes.add(createOptionBox(pollID , options.get(i).getOptionCaption() , RadioButtons.get(i), Perc.get(i)));
        }

        // If is the case of registered user, the user can vote.
        if(registered && active) {
            double total = sum;

            //add listener to update the ProgressBar when an option is selected
            optionGroup.selectedToggleProperty().addListener((observable, oldValue, newValue) -> {
                for(int i = 0 ; i < options.size() ; i++) {
                    String caption = RadioButtons.get(i).getText();
                    updateOptionBox(HBoxes.get(i), RadioButtons.get(i), total, caption, pollID);
                }
            });
        }else{
            // Otherwise, the unregistered user cannot vote.
            for(int i = 0 ; i < options.size() ; i++) {
                RadioButtons.get(i).setDisable(true);
            }
        }
        VBox.setMargin(questionLabel, new Insets(0, 0, 5, 0));
        form.getChildren().addAll(questionLabel);
        for(int i = 0 ; i < options.size() ; i++) {
            VBox.setMargin(HBoxes.get(i), new Insets(0, 5, 5, 5));
            form.getChildren().addAll(HBoxes.get(i));
        }
        return form;
    }
    // Method to create an HBox containing a ProgressBar and a Label for an option
    private HBox createOptionBox(int pollID , String optionCaption , RadioButton option, double value) {
        ProgressBar progressBar = new ProgressBar();
        progressBar.getStyleClass().add("progress-bar");
        progressBar.setProgress(value);

        this.progressList.add(new pollProgressBar(pollID , optionCaption , progressBar));

        Label percentageLabel = new Label();
        percentageLabel.setText(String.format("%.0f%%", this.progressList.get(this.progressList.size()-1).getProgressBar().getProgress() * 100));
        percentageLabel.getStyleClass().add("input-label");
        this.progressList.get(this.progressList.size()-1).setPercentageLabel(percentageLabel);

        HBox hbox = new HBox(10);
        // We align the progressBar on the right.
        Region spacingRegion = new Region();
        HBox.setHgrow(spacingRegion, Priority.ALWAYS);  // The second label to the right.
        HBox.setMargin(spacingRegion, new Insets(0, 0, 0, 40));

        hbox.getChildren().addAll(option, spacingRegion, this.progressList.get(this.progressList.size()-1).getProgressBar(),this.progressList.get(this.progressList.size()-1).getPercentageLabel());
        return hbox;
    }

    //method to update the ProgressBar and Label based on user selection
    private void updateOptionBox(HBox hbox, RadioButton option, double sum, String caption,int pollID) {

        if (option.isSelected()) {

            Poll p = new Poll();
            //we get the poll at which the user has voted, from redis
            PollRedisDAO pRedis = new PollRedisDAO();
            List<Poll> pollist = pRedis.getAllPollFromRedis();
            for (Poll poll : pollist) {
                if (poll.getPollID() == pollID) {
                    p = poll;
                    break;
                }
            }
            //we get the number of votes on that poll
            sum = p.getNumberOfVotes();
            //System.out.println("number of votes: "+ sum);

            // Creation of the object Poll.
            //System.out.println("voted caption: "+ caption);
            String oldVotedCaption = Session.getCustomerInfo().OptionPresent(pollID);
            //System.out.println("old voted caption: "+ oldVotedCaption);

            ProgressBar oldCaptionProgressBar = getCaptionProgressBar(oldVotedCaption);
            ProgressBar currentCaptionProgressBar = getCaptionProgressBar(caption);

            if (currentCaptionProgressBar == null) {
                return;
            }

            if (oldCaptionProgressBar == null) {
                //take the old value of the progress bar
                double oldValue= 0;
                int index = 0;
                for (int i = 0; i < p.getOptions().size();i++){
                    if(Objects.equals(p.getOptions().get(i).getOptionCaption(), caption)){
                        oldValue = p.getOptions().get(i).getOptionVotes();
                        index = i;

                        break;
                    }
                }

                //System.out.println("prima volta che si vota, vecchio valore: " + oldValue);
                pRedis.updatePollOptionVotes(pollID, new pollOption(caption), true);
                sum++;
                oldValue++;
                p.setNumberOfVotes(p.getNumberOfVotes()+1);
                p.getOptions().get(index).setOptionVotes((int)oldValue);
                //System.out.println("prima volta che si vota, nuovo valore: " + oldValue);
                currentCaptionProgressBar.setProgress(oldValue / sum);

                double percentage = Math.round(currentCaptionProgressBar.getProgress() * 100);
                //percentage = Math.round(percentage);
                //System.out.println("prima volta che si vota, percebtuale: " + percentage);
                //get index from progressList
                int i = 0;
                for (i = 0; i < this.progressList.size(); i++) {
                    if (Objects.equals(this.progressList.get(i).getPollCaption(), caption)) {
                        this.progressList.get(i).getPercentageLabel().setText(String.format("%.0f%%", percentage));
                        break;
                    }
                }

                // The suer has not voted in this poll yet (Ex-Novo).
            } else {

                //System.out.println("non è la prima volta che voto");
                // The user already voted in this poll.
                if (!oldVotedCaption.equals(caption)) {
                    // The voted option has changed.
                    //System.out.println("ho votato una roba diversa da prima");

                    //oldCaptionProgressBar = getCaptionProgressBar(oldVotedCaption);
                    double oldValue = 0 ;
                    int index = 0;
                    for (int i = 0; i < p.getOptions().size();i++){
                        if(Objects.equals(p.getOptions().get(i).getOptionCaption(), oldVotedCaption)){
                            oldValue = p.getOptions().get(i).getOptionVotes();
                            index = i;
                            break;
                        }
                    }

                    pRedis.updatePollOptionVotes(pollID, new pollOption(oldVotedCaption), false);
                    //System.out.println("old Value prima di decrementare: " + oldValue);
                    oldValue--;
                    p.getOptions().get(index).setOptionVotes((int)oldValue);
                    //System.out.println("numero totale di voti: " + sum);
                    oldCaptionProgressBar.setProgress(oldValue / sum);

                    double percentage = Math.round(oldCaptionProgressBar.getProgress() * 100);
                    //percentage = Math.round(percentage);
                    //System.out.println("percentage: " + percentage);
                    //int i = 0;
                    oldValue = 0;
                    index = 0;
                    for (int k = 0; k < p.getOptions().size();k++){
                        if(Objects.equals(p.getOptions().get(k).getOptionCaption(), caption)){
                            oldValue = p.getOptions().get(k).getOptionVotes();
                            index = k;
                            break;
                        }
                    }
                    oldValue++;
                    p.getOptions().get(index).setOptionVotes((int)oldValue);
                    currentCaptionProgressBar.setProgress(oldValue / sum);
                    double percentage2 = Math.round(currentCaptionProgressBar.getProgress() * 100);
                    //System.out.println("percentage2: " + percentage2);
                    //percentage2 = Math.round(percentage2);
                    for (int i = 0; i < this.progressList.size(); i++) {
                        if (Objects.equals(this.progressList.get(i).getPollCaption(), oldVotedCaption)) {
                            this.progressList.get(i).getPercentageLabel().setText(String.format("%.0f%%", percentage));
                        }
                        if (Objects.equals(this.progressList.get(i).getPollCaption(), caption)) {
                            this.progressList.get(i).getPercentageLabel().setText(String.format("%.0f%%", percentage2));
                        }
                    }

                    pRedis.updatePollOptionVotes(pollID, new pollOption(caption), true);

                } else {
                    
                    return;
                }
            }

            Session.getCustomerInfo().AddOption(new customerVotedPollVoice(pollID,caption));
            createUserCookie(Session.getCustomerInfo());

            //System.out.println("numero di opzioni"+ p.getOptions().size());
            for (int i = 0; i < p.getOptions().size(); i++) {
                if (Objects.equals(p.getOptions().get(i).getOptionCaption(), caption)) {
                    //System.out.println("opzione votata è già aggiornata");
                    continue;
                }
                if (oldVotedCaption != null) {
                    if (Objects.equals(p.getOptions().get(i).getOptionCaption(), oldVotedCaption)) {
                        //System.out.println("opzione votata prima è già aggiornata");
                        continue;
                    }
                }

                // The other options.
                ProgressBar OtherCaptionProgressBar = getCaptionProgressBar(p.getOptions().get(i).getOptionCaption());
                //double oldValue = (OtherCaptionProgressBar.getProgress() * sum);
                //System.out.println("nuovo valore progress bar non votate "+ oldValue/sum);
                OtherCaptionProgressBar.setProgress(p.getOptions().get(i).getOptionVotes() / sum);

                double percentage2 = Math.round(OtherCaptionProgressBar.getProgress() * 100);
                //percentage2 = Math.round(percentage2);
                this.getCaptionLabel(p.getOptions().get(i).getOptionCaption()).setText(String.format("%.0f%%", percentage2));
            }
        }
            // Update the poll in Redis.

    }

    private ProgressBar getCaptionProgressBar(String caption){
        for (it.unipi.dii.pagesGUI.pollProgressBar pollProgressBar : this.progressList) {
            if (Objects.equals(pollProgressBar.getPollCaption(), caption)) {
                return pollProgressBar.getProgressBar();
            }
        }
        return null;
    }
    private Label getCaptionLabel(String caption){
        for (it.unipi.dii.pagesGUI.pollProgressBar pollProgressBar : this.progressList) {
            if (Objects.equals(pollProgressBar.getPollCaption(), caption)) {
                return pollProgressBar.getPercentageLabel();
            }
        }
        return null;
    }
}