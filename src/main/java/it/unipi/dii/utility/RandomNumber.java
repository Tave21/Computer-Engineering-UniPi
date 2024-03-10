package it.unipi.dii.utility;

import java.util.Random;

import static java.lang.Math.floor;
import static java.lang.Math.pow;

public class RandomNumber {
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
        return -mean * Math.log(1 - random.nextDouble()); // Applica la trasformazione inversa per la distribuzione esponenziale
    }

    /**
     * @param num The number to truncate.
     * @param how_much How many numbers after the comma you need to cut.
     * @return The truncate number.
     */

    public static double truncateNumber2(Double num, int how_much) {
        if(how_much <= 0){
            return Math.floor(num);
        }

        StringBuilder res = new StringBuilder();
        String input = num.toString();

        boolean afterComma = false;
        int count = 0;

        for (char character : input.toCharArray()) {
            if (afterComma && count < how_much) {
                res.append(character);
                count++;
            } else if (character == '.') {
                afterComma = true;
                res.append(character);
            } else if (!afterComma) {
                res.append(character);
            }
        }

        return Double.parseDouble(res.toString());
    }

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
