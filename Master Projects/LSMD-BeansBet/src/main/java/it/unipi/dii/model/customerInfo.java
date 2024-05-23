package it.unipi.dii.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class customerInfo {
    private List<customerVotedPollVoice> voices;
    public customerInfo(List<customerVotedPollVoice> voices) {
        this.voices = voices;
    }
    public customerInfo(){
        this.voices = new ArrayList<>();
    }

    @JsonProperty("voices")
    public List<customerVotedPollVoice> getVoices() {
        return voices;
    }
    public void setVoices(List<customerVotedPollVoice> voices) {
        this.voices = voices;
    }


    /**
     * Insert a new option when a user votes in a poll
     * @param voice is an object that contains the pollID and the caption voted by the user
     */
    public void AddOption(customerVotedPollVoice voice){
        for (customerVotedPollVoice customerVotedPollV : this.voices) {
            if (customerVotedPollV.getPollID() == voice.getPollID()) {
                if (!Objects.equals(customerVotedPollV.getVotedOptionCaption(), voice.getVotedOptionCaption())) {
                    customerVotedPollV.setVotedOptionCaption(voice.getVotedOptionCaption());
                }
                return;
            }
        }
        this.voices.add(voice);
    }
    /**
     * Check if a user has already voted for that poll
     * @param pollID the id of the poll
     * @return the caption voted by the user if it has already voted for that poll
     * otherwise returns null
     */
    public String OptionPresent(int pollID){
        for (customerVotedPollVoice voice : this.voices) {
            if (voice.getPollID() == pollID) {
                return voice.getVotedOptionCaption();
            }
        }
        return null;
    }
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        customerInfo that = (customerInfo) o;
        return Objects.equals(voices, that.voices);
    }
    @Override
    public int hashCode() {
        return Objects.hash(voices);
    }
    @Override
    public String toString() {
        return "{voices:" + voices + "}";
    }
}