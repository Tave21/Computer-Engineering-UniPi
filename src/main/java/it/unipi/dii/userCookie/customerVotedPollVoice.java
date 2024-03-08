package it.unipi.dii.userCookie;

import com.fasterxml.jackson.annotation.JsonProperty;

public class customerVotedPollVoice {
    private int pollID; // The REDIS pollID.
    private String votedOptionCaption;

    public customerVotedPollVoice(int pollID, String votedOptionCaption) {
        this.pollID = pollID;
        this.votedOptionCaption = votedOptionCaption;
    }

    public customerVotedPollVoice(){
    }

    @JsonProperty("pollID")
    public int getPollID() {
        return this.pollID;
    }

    public void setPollID(int pollID) {
        this.pollID = pollID;
    }

    @JsonProperty("votedOptionCaption")
    public String getVotedOptionCaption() {
        return this.votedOptionCaption;
    }
    public void setVotedOptionCaption(String value) {
        this.votedOptionCaption = value;
    }
}
