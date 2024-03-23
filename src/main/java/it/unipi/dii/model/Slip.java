package it.unipi.dii.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.dateTimes.getCurrentInstant;
import static it.unipi.dii.utility.regularExpressionChecks.checkTimestampFormat;

public class Slip {
    private final static int MIN_BETAMOUNT = 2; // Minimum amount of credit required to place a bet.
    private final static int MAX_AMOUNT = 3000; // maximum winning quantity.
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
        this.win = -1;
        this.amount = 0;
    }

    public Slip() {
        this.win = -1;
        this.amount = 0;
    }

    /**
     * @param betAmount The target bet amount.
     * @return True if the input value is bigger than the minimum value for a bet amount.
     */
    public static boolean checkBetAmount(double betAmount) {
        return betAmount >= MIN_BETAMOUNT;
    }

    @JsonProperty("slipID")
    public Integer getSlipID() {
        return this.slipID;
    }

    @JsonProperty("username")
    public String getUsername() {
        return this.username;
    }

    @JsonProperty("confirmationDate")
    public String getConfirmationDate() {
        return this.confirmationDate;
    }

    @JsonProperty("creationDate")
    public String getCreationDate() {
        return this.creationDate;
    }

    @JsonProperty("amount")
    public double getAmount() {
        return this.amount;
    }

    @JsonProperty("betAmount")
    public double getBetAmount() {
        return this.betAmount;
    }

    @JsonProperty("win")
    public Integer getWin() {
        return this.win;
    }

    @JsonProperty("betsList")
    public List<Bet> findBetsList() {
        return this.betsList;
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
        if (amount > MAX_AMOUNT) {
            this.amount = MAX_AMOUNT;
        } else {
            this.amount = amount;
        }
    }

    public void setBetAmount(double bet_amount) {
        if (bet_amount >= MIN_BETAMOUNT) {
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
     * @return True if al the bets in the list have win = -1.
     */
    public boolean betListNotEvaluated() {
        for (Bet bet : this.betsList) {
            if (bet.getWin() != -1) {
                return false;
            }
        }
        return true;
    }


    /**
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

    public void setBetsWinToMinus1() {
        for (Bet bet : this.betsList) {
            bet.setWin(-1); // The bet is not evaluated yet.
        }
        this.setWin(-1); // The slip is not evaluated yet.
        this.setAmount(0);
    }

    /**
     * Evaluate the slips by seeing the bet results.
     */
    public void checkIfThisSlipIsWin(){
        for (Bet bet : this.betsList) {
            if (bet.getWin() == -1) {
                this.setWin(-1);
                this.setAmount(0);
                return;
            } else if (bet.getWin() == 0) {
                this.setWin(0);
                this.setAmount(0);
                return;
            }
        }
        this.computeTotal();
        this.setWin(1);
    }

    /**
     * A set of checks that must be done before the inserting operation of a slip in MongoDB.
     *
     * @return True if the slip is valid to be inserted in the database.
     */
    public boolean checkSlipValidity() {
        if (
                this.betsList.isEmpty()
                        || !this.betListNotEvaluated()
                        || !checkBetAmount(this.getBetAmount())
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
        for (Bet bet : this.betsList) {
            dateToCheckCreation = Instant.parse(bet.getMatchDate());
            if (!dateToCheckConfirmation.isBefore(dateToCheckCreation)) {
                return false;
            }
        }
        return true;
    }

    /**
     * create a string with all atributes of the Slip
     *
     * @return a string in a JSON format
     */
    @Override
    public String toString() {
        return "Slip{" + "SlipID = " + slipID + ", username = '" + username + '\'' + ", creationDate = '" + creationDate + '\'' + ", confirmationDate = '" + confirmationDate + '\'' + ", amount = " + amount + ", betAmount = " + betAmount + ", win = " + win + ", betsList = " + betsList + '}';
    }


    /**
     * Check if a slip is equals to another one
     *
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
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return Objects.hash(slipID, username, creationDate, confirmationDate, amount, betAmount, win, betsList);
    }


}
