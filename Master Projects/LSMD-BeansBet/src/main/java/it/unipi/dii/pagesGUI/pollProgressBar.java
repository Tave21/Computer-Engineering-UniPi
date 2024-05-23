package it.unipi.dii.pagesGUI;

import com.fasterxml.jackson.annotation.JsonProperty;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;

import java.util.Objects;

public class pollProgressBar {
    private int pollID;
    private String pollCaption;
    private ProgressBar progressBar;
    private Label percentageLabel;

    public pollProgressBar(int pollID, String pollCaption, ProgressBar progressBar) {
        this.pollID = pollID;
        this.pollCaption = pollCaption;
        this.progressBar = progressBar;
    }

    public pollProgressBar(int pollID, String pollCaption, ProgressBar progressBar, Label percentageLabel) {
        this.pollID = pollID;
        this.pollCaption = pollCaption;
        this.progressBar = progressBar;
        this.percentageLabel = percentageLabel;
    }

    public void setPercentageLabel(Label percentageLabel) {
        this.percentageLabel = percentageLabel;
    }
    public Label getPercentageLabel() {
        return percentageLabel;
    }

    public pollProgressBar(){

    }
    @JsonProperty("pollID")
    public int getPollID() {
        return pollID;
    }

    public void setPollID(int pollID) {
        this.pollID = pollID;
    }
    @JsonProperty("pollCaption")
    public String getPollCaption() {
        return pollCaption;
    }

    public void setPollCaption(String pollCaption) {
        this.pollCaption = pollCaption;
    }
    @JsonProperty("progressBar")
    public ProgressBar getProgressBar() {
        return progressBar;
    }

    public void setProgressBar(ProgressBar progressBar) {
        this.progressBar = progressBar;
    }

    @Override
    public int hashCode() {
        return Objects.hash(pollID, pollCaption, progressBar);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        pollProgressBar that = (pollProgressBar) o;
        return pollID == that.pollID &&
                Objects.equals(pollCaption, that.pollCaption) &&
                Objects.equals(progressBar, that.progressBar);
    }

    @Override
    public String toString() {
        return "CustomerVotedPollVoice{" +
                "pollID=" + pollID +
                ", pollCaption='" + pollCaption + '\'' +
                ", progressBar=" + progressBar +
                '}';
    }
}
