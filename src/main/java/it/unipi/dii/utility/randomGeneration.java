package it.unipi.dii.utility;
import it.unipi.dii.model.pollOption;

import static it.unipi.dii.utility.RandomNumber.*;

public class randomGeneration {
    /**
     * Special function used for generate a random match multiplier.
     * The used random distribution is exponential.
     * @param lambda The mean value of the distribution.
     * @return A random match multiplier.
     */
    public static double generateMultiplier(double lambda){
        final double c = truncateNumber(generateExponentialDistributedNumber(lambda) , 3);
        if(c <= 1.1){
            return 1.1;
        }else{
            if(c > 10){
                return 10;
            }else{
                return c;
            }
        }
    }
    /**
     * Special function used for generate a random sum of money.
     * The used random distribution is exponential.
     * @param lambda The mean value of the distribution.
     * @return A random sum of money.
     */
    public static double generateSumOfMoney(double lambda){
        double c = truncateNumber(generateExponentialDistributedNumber(lambda) , 2);
        if(c < 2){
            return 2;
        }else{
            return c;
        }
    }
    /**
     * Special function used for generate a random cellphone number.
     * @param prefix The number prefix to apply.
     * @return A random cellphone number.
     */
    public static String generatePhoneNumber(String prefix) {
        // Generate a random 9-digit number.
        String phoneNumber = "" + generateRandomNaturalNumber(1 , 9);
        for(int i = 1; i <= 9; i++) {
            phoneNumber = phoneNumber + generateRandomNaturalNumber(0 , 9);
        }
        return prefix + " " + phoneNumber;
    }

    public static void optionRandomizeVotes(pollOption pollOpt){
        for(int i = 0 ; i < generateRandomNaturalNumber(1 , 40) ; i++){
            pollOpt.voteOption();
        }

    }
}
