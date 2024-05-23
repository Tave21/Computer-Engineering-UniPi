package it.unipi.dii.utility;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;

public class securityLibrary {
    /**
     * This function calculate the digest of a string using the SHA256 algorithm.
     * @param input The string to convert to hash.
     * @return the digest.
     */
    public static String calculateSHA256(String input) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(input.getBytes(StandardCharsets.UTF_8));
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1){
                    hexString.append('0');
                }
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            return null;
        }
    }
    /**
     * This function check if the input digest is equal to an input SHA256 digest.
     * @param inputString The password to test.
     * @param digest The digest to test.
     * @return True if input string's digest is equal to the input digest.
     */
    public static boolean CheckHash(String inputString, String digest){
        return (Objects.equals(calculateSHA256(inputString), digest));
    }

}
