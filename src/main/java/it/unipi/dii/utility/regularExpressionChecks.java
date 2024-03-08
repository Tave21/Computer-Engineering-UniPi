package it.unipi.dii.utility;

import java.util.regex.Pattern;

public class regularExpressionChecks {
    /**
     *
     * @param stringToCheck The string to check.
     * @return True if the string contains something in the format "aaa@bbb.cc".
     */
    public static boolean isValidEmail(String stringToCheck) {
        return stringToCheck.matches("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b");
    }

    /**
     *
     * @param stringToCheck The string to check.
     * @return True if the string is a valid cellphone number (exactly 10 numbers).
     */

    public static boolean isValidPhoneNumber(String stringToCheck) {
        return stringToCheck.matches("^[0-9]{10}$"); // Only numbers between 1 and 10 digits
    }

    /**
     *
     * @param stringToCheck The String to check.
     * @return True if the length is inside the bounds.
     */

    public static boolean isCorrectLength(String stringToCheck , int min , int max) {
        return stringToCheck.length() >= min && stringToCheck.length() <= max;
    }

    /**
     *
     * @param stringToCheck The String to check.
     * @return True if the string contains only letters [a - A].
     */
    public static boolean isOnlyLetters(String stringToCheck , boolean whiteSpace) {
        if(whiteSpace){
            return Pattern.matches("^[a-zA-Z ]+$", stringToCheck);
        }else{
            return Pattern.matches("^[a-zA-Z]+$", stringToCheck);
        }
    }
    /**
     *
     * @param stringToCheck The String to check.
     * @param whiteSpace Set to true sif white space are accepted.
     * @return True if the string contains only letters and/or numbers.
     */
    public static boolean isOnlyLettersAndNumbers(String stringToCheck , boolean whiteSpace) {
        if(whiteSpace) {
            return Pattern.matches("^[a-zA-Z0-9 ]+$", stringToCheck);
        }else{
            return Pattern.matches("^[a-zA-Z0-9]+$", stringToCheck);
        }
    }
    /**
     *
     * @param number  The number to check.
     * @return True is the number is a NaN.
     */
    public static boolean isNaN(double number){
        return Double.isNaN(number);
    }

}
