package it.unipi.dii.utility;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.List;

public class PasswordGenerator {
    private static final String LOWERCASE_CHARACTERS = "abcdefghijklmnopqrstuvwxyz";
    private static final String UPPERCASE_CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    private static final String DIGITS = "0123456789";
    private static final String SPECIAL_CHARACTERS = "!@#$%^&*()-_=+|:<>?";
    private static final List<String> SETS = Arrays.asList(
            LOWERCASE_CHARACTERS,
            UPPERCASE_CHARACTERS,
            DIGITS,
            SPECIAL_CHARACTERS
    );

    /**
     * Generate a random password of a given length.
     * @param length The length of the wanted password.
     * @return A string of length characters.
     */
    public static String generateRandomPassword(int length) {
        if (length <= 0) {
            throw new IllegalArgumentException("Password length must be greater than 0");
        }

        StringBuilder password = new StringBuilder();
        SecureRandom random = new SecureRandom();

        // Generate the remaining characters
        for (int i = 0; i < length; i++) {
            password.append(getRandomChar(getRandomCharSet(random), random));
        }

        // Shuffle the password characters
        char[] passwordArray = password.toString().toCharArray();
        for (int i = passwordArray.length - 1; i > 0; i--) {
            int index = random.nextInt(i + 1);
            char temp = passwordArray[index];
            passwordArray[index] = passwordArray[i];
            passwordArray[i] = temp;
        }
        return new String(passwordArray);
    }

    /**
     * @param characterSet The set of characters.
     * @param random The random number generator.
     * @return A random character from the set.
     */
    private static char getRandomChar(String characterSet, SecureRandom random) {
        int index = random.nextInt(characterSet.length());
        return characterSet.charAt(index);
    }

    /**
     * @param random The random number generator.
     * @return Choose one of the characters sets.
     */
    private static String getRandomCharSet(SecureRandom random) {
        return SETS.get(random.nextInt(SETS.size() - 1));
    }

}
