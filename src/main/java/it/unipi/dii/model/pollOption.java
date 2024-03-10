package it.unipi.dii.model;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;

public class pollOption {
    String optionCaption; // The text caption of the option.
    int optionVotes; // How many votes this option has.
    public pollOption(){
        optionVotes = 0;
    }
    public pollOption(String caption){
        this.optionCaption = caption;
        this.optionVotes = 0;
    }
    @JsonProperty("optionCaption")
    public String getOptionCaption(){
        return this.optionCaption;
    }
    public void setOptionCaption(String caption){
        this.optionCaption = caption;
    }

    @JsonProperty("optionVotes")
    public int getOptionVotes(){
        return this.optionVotes;
    }
    public void setOptionVotes(Integer vote){
        this.optionVotes = vote;
    }

    @Override
    public int hashCode() {
        return Objects.hash(optionCaption, optionVotes);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        pollOption that = (pollOption) o;
        return optionVotes == that.optionVotes &&
                Objects.equals(optionCaption, that.optionCaption);
    }

    /**
     * Add a vote to this option.
     */

    public void voteOption(){
        this.optionVotes++;
    }

    /**
     * Add howMuch votes to the option.
     * @param howMuch How many vote to add.
     */
    public void multipleVoteOption(int howMuch){
        this.optionVotes = this.optionVotes + howMuch;
    }
    /**
     * Remove a vote of this option.
     */
    public void unvoteOption(){
        this.optionVotes--;
    }

    /**
     * Subtract howMuch votes to the option.
     * @param howMuch How many vote to subtract.
     */
    public void multipleUnvoteOption(int howMuch){
        this.optionVotes = this.optionVotes - howMuch;
    }
}
