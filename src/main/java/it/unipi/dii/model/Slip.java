package it.unipi.dii.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.DateTimes.checkTimestampFormat;
import static it.unipi.dii.utility.DateTimes.getCurrentInstant;

public class Slip {
    private final int MIN_BETAMOUNT = 2; // Minimum amount of credit required to place a bet.
    private final int MAX_AMOUNT = 3000; // maximum winning quantity.
    private Integer slipID;
    private String username;
    private String creationDate;
    private String confirmationDate;
    private double amount; // The amount of money that the customer would win.
    private double betAmount; // The amount of money that the customer bet.
    private Integer win;
    public List<Bet> betsList = new ArrayList<>();

    // Constructor
    public Slip(String username, String confirmationDate, String creationDate, double betAmount) {
        this.username = username;
        this.creationDate = creationDate;
        this.confirmationDate = confirmationDate;
        this.betAmount = betAmount;
        this.amount = 0;
        this.win = -1;
    }

    public Slip() {
        this.win = -1;
        this.amount = 0;
    }

    /**
     *
     * @param betAmount The target bet amount.
     * @return True if the input value is bigger than the minimum value for a bet amount.
     */
    public boolean checkBetAmount(double betAmount) {
        return this.betAmount >= this.MIN_BETAMOUNT;
    }

    @JsonProperty("slipID")
    public Integer getSlipID() {
        return slipID;
    }

    @JsonProperty("username")
    public String getUsername() {
        return username;
    }

    @JsonProperty("confirmationDate")
    public String getConfirmationDate() {
        return confirmationDate;
    }

    @JsonProperty("creationDate")
    public String getCreationDate() {
        return creationDate;
    }

    @JsonProperty("amount")
    public double getAmount() {
        return amount;
    }

    @JsonProperty("betAmount")
    public double getBetAmount() {
        return this.betAmount;
    }

    @JsonProperty("win")
    public Integer getWin() {
        return win;
    }

    @JsonProperty("betsList")
    public List<Bet> findBetsList() {
        return betsList;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setSlipID(Integer slipID) {
        this.slipID = slipID;
    }

    public void setConfirmationDate(String confirmationDate) {
        this.confirmationDate = confirmationDate;
    }

    public void setCreationDate(String creationDate) {
        this.creationDate = creationDate;
    }

    public void setAmount(double amount) {
        if(amount > this.MAX_AMOUNT){
            this.amount = this.MAX_AMOUNT;
        }else {
            this.amount = amount;
        }
    }

    public void setBetAmount(double bet_amount) {
        if (bet_amount >= this.MIN_BETAMOUNT) {
            this.betAmount = bet_amount;
        }
    }

    public void setWin(int win) {
        if (win == 1 || win == 0 || win == -1) {
            this.win = win;
        }
    }

    public void setBetsList(List<Bet> betsList) {
        this.betsList = betsList;
    }

    /**
     * Compute the potential winning quantity.
     */
    public void computeTotal() { // Compute the eventual total win of a slip.
        this.setAmount(this.betAmount * this.computeTotalMultiplicator());
    }

    /**
     *
     * @param matchID the target match.
     * @return The index of the bet related to the target match, or -1 if
     * There aren't any bets related to the target match.
     */

    public int findBetIndex(int matchID) {
        if(matchID >= 0) {
            for (int i = 0; i < this.betsList.size(); i++) {
                if (this.betsList.get(i).getMatchID().equals(matchID)) {
                    // is the Bet is present in the slip.
                    return i;
                }
            }
        }
        return -1; // if is not present.
    }

    /**
     *
     * @return True if al the bets in the list have win = -1.
     */
    public boolean betListNotEvaluated(){
        for (Bet bet : this.betsList) {
            if (bet.getWin() != -1) {
                return false;
            }
        }
        return true;
    }


    /**
     *
     * @return The total multiplier of the slip.
     */

    public double computeTotalMultiplicator() {
        double total = 1;
        for (Bet bet : betsList) {
            total = total * bet.getChosenMultiplierValue();
        }
        return total;
    }

    /**
     * Set the win field to -1 to all the bets of the slip and to the slip.
     * Moreover, it set amount to 0.
     */

    public void setBetsWinToMinus1(){
        for (Bet bet : this.betsList) {
            bet.setWin(-1); // The bet is not evaluated yet.
        }
        this.setWin(-1); // The slip is not evaluated yet.
        this.setAmount(0);
    }

    /**
     * A set of checks that must be done before the inserting operation of a slip in MongoDB.
     * @return True if the slip is valid to be inserted in the database.
     */
    public boolean checkSlipValidity() {
        if (
                this.betsList.isEmpty()
                        || !this.betListNotEvaluated()
                        || !this.checkBetAmount(this.getBetAmount())
                        || this.getAmount() != 0
                        || this.getWin() != -1
                        || this.getUsername().isEmpty()
                        || !checkTimestampFormat(this.getCreationDate())
                        || !checkTimestampFormat(this.getConfirmationDate())

        ) {
            return false;
        }
        Instant dateToCheckCreation = Instant.parse(this.getCreationDate());
        Instant dateToCheckConfirmation = Instant.parse(this.getConfirmationDate());
        Instant now = getCurrentInstant();

        // If the two dates are after the current instant, then the slip is not valid.
        if (now.isBefore(dateToCheckCreation) || now.isBefore(dateToCheckConfirmation.minusSeconds(1)) || dateToCheckConfirmation.isBefore(dateToCheckCreation)) {

            return false;
        }
        // We do some variable recycling.
        for (int i = 0; i < this.betsList.size(); i++) {
            dateToCheckCreation = Instant.parse(this.betsList.get(i).getMatchDate());
            if (!dateToCheckConfirmation.isBefore(dateToCheckCreation)) {
                return false;
            }
        }
        return true;
    }

    /**
     * create a string with all atributes of the Slip
     * @return a string in a JSON format
     */
    @Override
    public String toString() {
        return "Slip{" + "SlipID = " + slipID + ", username = '" + username + '\'' + ", creationDate = '" + creationDate + '\'' + ", confirmationDate = '" + confirmationDate + '\'' + ", amount = " + amount + ", betAmount = " + betAmount + ", win = " + win + ", betsList = " + betsList + '}';
    }


    /**
     * Check if a slip is equals to another one
     * @param o is a slip
     * @return true if the two slips are equal, otherwise false
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Slip slip = (Slip) o;
        return Double.compare(slip.amount, amount) == 0 &&
                Double.compare(slip.betAmount, betAmount) == 0 &&
                Objects.equals(slipID, slip.slipID) &&
                Objects.equals(username, slip.username) &&
                Objects.equals(creationDate, slip.creationDate) &&
                Objects.equals(confirmationDate, slip.confirmationDate) &&
                Objects.equals(win, slip.win) &&
                Objects.equals(betsList, slip.betsList);
    }

    /**
     * create a hash for the admin
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return Objects.hash(slipID, username, creationDate, confirmationDate, amount, betAmount, win, betsList);
    }


}
