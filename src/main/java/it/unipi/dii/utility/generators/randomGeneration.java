package it.unipi.dii.utility.generators;
import java.util.Random;
import static java.lang.Math.floor;
import static java.lang.Math.pow;

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
        StringBuilder phoneNumber = new StringBuilder("" + generateRandomNaturalNumber(1, 9));
        for(int i = 1; i <= 9; i++) {
            phoneNumber.append(generateRandomNaturalNumber(0, 9));
        }
        return prefix + " " + phoneNumber;
    }

    /**
     * @param min Lower bound.
     * @param max Upper bound.
     * @return A long (without comma) number between min and max, generated through a uniform distribution.
     */
    public static long generateRandomNaturalNumber(long min, long max) {
        Random random = new Random();
        return (random.nextLong(max - min + 1) + min);
    }
    /**
     * @param min Lower bound.
     * @param max Upper bound.
     * @return A real number between min and max, generated through a uniform distribution.
     */
    public static double generateRandomDoubleNumber(double min, double max) {
        Random random = new Random();
        return (random.nextDouble(max - min + 1) + min);
    }

    /**
     * @param mean The mean number.
     * @return A double number, generated through an exponential distribution.
     */
    public static double generateExponentialDistributedNumber(double mean) {
        Random random = new Random();
        return -mean * Math.log(1 - random.nextDouble());
    }

    /**
     * @param num The number to truncate.
     * @param how_much How many numbers after the comma you need to cut.
     * @return The truncate number.
     */
    public static double truncateNumber(Double num, int how_much) {
        if(how_much == 0){
            return floor(num);
        }else if( how_much == 1){
            return Math.floor(num * 10) / 10;
        }else if(how_much == 2){
            return Math.floor(num * 100) / 100;
        }else if (how_much == 3){
            return Math.floor(num * 1000) / 1000;
        }else if(how_much > 3){
            final int pow10 = (int) pow(10, how_much);
            return Math.floor(num * pow10) / pow10;
        } else{
            return num;
        }
    }
}
