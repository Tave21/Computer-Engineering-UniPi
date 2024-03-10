package it.unipi.dii.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Objects;

public class Poll {
    private Integer pollID;
    private String pollName; // It is the caption of the poll (e.g. "What is your favourite team?") ?
    private String pollType;
    private String creationDate;
    private String activationDate;
    private List<pollOption> options;
    private Integer numberOfVotes; // How many people have voted in this poll.

    public Poll(){
        this.numberOfVotes = 0;
    }
    public Poll( String pollName, String pollType, String creationDate, String activationDate) {
        this.pollName = pollName;
        this.creationDate = creationDate;
        this.pollType = pollType;
        this.activationDate = activationDate;
        this.numberOfVotes = 0;
    }

    public void setOptions(List<pollOption> list){
        this.options = list;
        UpdateNumberOfVotes();
    }

    /**
     * This function tail-inserts a new option in the poll.
     * @param op The option to add.
     */

    public void addOption(pollOption op){
        this.options.add(op);
    }

    @JsonProperty("pollID")
    public Integer getPollID() {
        return pollID;
    }
    public void setPollID(Integer pollID) {
        this.pollID = pollID;
    }
    @JsonProperty("pollName")
    public String getPollName() {
        return pollName;
    }

    public void setPollName(String pollName) {
        this.pollName = pollName;
    }

    @JsonProperty("pollType")
    public String getPollType() {
        return pollType;
    }

    public void setPollType(String pollType) {
        this.pollType = pollType;
    }

    @JsonProperty("creationDate")
    public String getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(String creationDate) {
        this.creationDate = creationDate;
    }

    @JsonProperty("options")
    public List<pollOption> getOptions() {
        return this.options;
    }
    @JsonProperty("activationDate")
    public String getActivationDate() {
        return this.activationDate;
    }
    public void setActivationDate(String date){
        this.activationDate = date;
    }

    @JsonProperty("numberOfVotes")
    public Integer getNumberOfVotes(){
        return this.numberOfVotes;
    }
    public void setNumberOfVotes(Integer numberOfVotes){
        UpdateNumberOfVotes();
    }


    /**
     * This function update the numberOfVotes field to the current situation.
     */
    public void UpdateNumberOfVotes(){ //numero di voti totali aggiornato
        this.numberOfVotes = 0;
        for(int i = 0; i < this.options.size(); i++){
            this.numberOfVotes = this.numberOfVotes + this.options.get(i).optionVotes;
        }
    }

    /**
     * Check if a poll is equals to another one
     * @param o is a poll
     * @return true if the two polls are equal, otherwise false
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Poll poll = (Poll) o;
        return Objects.equals(pollID, poll.pollID) &&
                Objects.equals(pollName, poll.pollName) &&
                Objects.equals(pollType, poll.pollType) &&
                Objects.equals(creationDate, poll.creationDate) &&
                Objects.equals(activationDate, poll.activationDate) &&
                Objects.equals(options, poll.options) &&
                Objects.equals(numberOfVotes, poll.numberOfVotes);
    }

    /**
     * create a hash for the admin
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return Objects.hash(pollID, pollName, pollType, creationDate, activationDate, options, numberOfVotes);
    }



}
