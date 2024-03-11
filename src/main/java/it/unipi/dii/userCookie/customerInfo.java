package it.unipi.dii.userCookie;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class customerInfo {
    private String username;
    private List<customerVotedPollVoice> voices;

    public customerInfo(String username, List<customerVotedPollVoice> voices) {
        this.username = username;
        this.voices = voices;
    }

    public customerInfo(){
        this.voices = new ArrayList<>();
    }

    @JsonProperty("username")
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    @JsonProperty("voices")
    public List<customerVotedPollVoice> getVoices() {
        return voices;
    }

    public void setVoices(List<customerVotedPollVoice> voices) {
        this.voices = voices;
    }

    public int AddOption(int pollID ,String caption){
        return this.AddOption(new customerVotedPollVoice(pollID , caption));
    }

    /**
     * Insert a new option when a user votes in a poll
     * @param voice is an object that contains the pollID and the caption voted by the user
     * @return 2 if the user has already voted the option of the poll, 1 if it has already voted
     * another option of that poll, 0 if it has never voted in that poll before.
     */
    public int AddOption(customerVotedPollVoice voice){
        for (it.unipi.dii.userCookie.customerVotedPollVoice customerVotedPollV : this.voices) {
            if (customerVotedPollV.getPollID() == voice.getPollID()) {
                if(Objects.equals(customerVotedPollV.getVotedOptionCaption(), voice.getVotedOptionCaption())){
                    return 2;
                }else {
                    customerVotedPollV.setVotedOptionCaption(voice.getVotedOptionCaption());
                    return 1;
                }
            }
        }
        this.voices.add(voice);
        return 0;
    }

    public void RemoveOption(int pollID){
        for (int i = 0 ; i < this.voices.size() ; i++) {
            if (this.voices.get(i).getPollID() == pollID) {
                this.voices.remove(i);
                return;
            }
        }
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
        return Objects.equals(username, that.username) &&
                Objects.equals(voices, that.voices);
    }
    @Override
    public int hashCode() {
        return Objects.hash(username, voices);
    }

    @Override
    public String toString() {
        return "{"+
                "username:'" + username + '\'' +
                ", voices:" + voices +
                '}';
    }
}