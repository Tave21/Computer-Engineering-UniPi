package it.unipi.dii.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.dateTimes.getCurrentInstant;
import static it.unipi.dii.utility.regularExpressionChecks.checkTimestampFormat;
import static java.lang.Math.abs;

public class Slip {
    private final static int MIN_BETAMOUNT = 2; // Minimum amount of credit required to place a bet.
    private final static int MAX_AMOUNT = 3000; // Maximum winning quantity.
    private Integer slipID;
    private String username;
    private String creationDate;
    private String confirmationDate;
    private double amount; // The amount of money that the customer would win.
    private double betAmount; // The amount of money that the customer bet.
    private Integer win;
    public List<Bet> betsList = new ArrayList<>();

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
            this.amount = abs(amount);
        }
    }

    public void setBetAmount(double bet_amount) {
        if (bet_amount >= MIN_BETAMOUNT) {
            this.betAmount = abs(bet_amount);
        }
    }

    public void setWin(int win) {
        if (win == 1 || win == 0 || win == -1) {
            // Only accepted values.
            this.win = win;
        }
    }

    public void setBetsList(List<Bet> betsList) {
        this.betsList = betsList;
    }

    /**
     * Compute the potential winning quantity and set the value in the amount field.
     */
    public void computeTotal() {
        this.setAmount(this.betAmount * this.computeTotalMultiplicator());
    }


    /**
     * @return True if al the bets in the list have win = -1, False instead.
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
     * Set the win field to -1 to all the bets of the slip and to the slip too.
     * Moreover, it set the amount of this slip to 0.
     */

    public void setBetsWinToMinus1() {
        for (Bet bet : this.betsList) {
            bet.setWin(-1); // The bet is not evaluated yet.
        }
        this.setWin(-1); // The slip is not evaluated yet.
        this.setAmount(0); // Set the amount to 0.
    }

    /**
     * Evaluate the slip by seeing the bet results.
     * It also set the win field of the slip to:
     *  <ul>
     *      <li>1 the slip has been win.</li>
     *      <li>0 the slip has been lost.</li>
     *      <li>-1 if the slip cannot be evaluated yet.</li>
     *  </ul>
     *  This function do not modify the bets in any way.
     */
    public void checkIfThisSlipIsWin(){
        for (Bet bet : this.betsList) {
            // For each bet in the slip.
            if (bet.getWin() == -1) {
                // The slip can't be evaluated yet.
                this.setWin(-1);
                this.setAmount(0);
                return;
            } else if (bet.getWin() == 0) {
                // The slip is surely lost, because a bet is lost.
                this.setWin(0);
                this.setAmount(0);
                return;
            }
        }
        // The slip is win.
        this.computeTotal(); // Compute the winning amount.
        this.setWin(1);
    }

    /**
     * A set of checks that must be done before the inserting operation of a slip in MongoDB.
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

    @Override
    public String toString() {
        return "Slip{" + "SlipID = " + slipID + ", username = '" + username + '\'' + ", creationDate = '" + creationDate + '\'' + ", confirmationDate = '" + confirmationDate + '\'' + ", amount = " + amount + ", betAmount = " + betAmount + ", win = " + win + ", betsList = " + betsList + '}';
    }

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

    @Override
    public int hashCode() {
        return Objects.hash(slipID, username, creationDate, confirmationDate, amount, betAmount, win, betsList);
    }


}
