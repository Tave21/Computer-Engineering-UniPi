package it.unipi.dii.userCookie;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.File;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.WriteJsonToFile.writeToJsonFile;

public class usernameCookie {
    private static final String PATH = "src/main/java/it/unipi/dii/userCookie/cookies/";
    private static final String PATH_PREFIX = "info_";

    /**
     * Create a cookie in a separate file.
     * @param customer Info of the customer.
     */
    public static void createUserCookie(customerInfo customer) {
        String fileName = PATH + PATH_PREFIX + customer.getUsername();
        List<customerInfo> clist = new ArrayList<>();
        clist.add(customer);
        writeToJsonFile(clist, fileName);
    }

    /**
     * @param username The target username.
     * @return The info of the customer.
     */

    public static customerInfo getVotedList(String username) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            File jsonFile = new File(PATH + PATH_PREFIX + username);
            customerInfo[] customerInfos = objectMapper.readValue(jsonFile, customerInfo[].class);
            return customerInfos[0];
        } catch (IOException e) {
            return null;
        }
    }

    /**
     *
     * @param username The target username.
     * @param pollID The target poll.
     * @return The caption that the user voted in that poll.
     * Or null if the user do not have voted in this poll.
     */

    public static String getVotedListCaption(String username , int pollID) {
        List<customerVotedPollVoice> cv = Objects.requireNonNull(getVotedList(username)).getVoices();
        for (it.unipi.dii.userCookie.customerVotedPollVoice customerVotedPollVoice : cv) {
            if (customerVotedPollVoice.getPollID() == pollID) {
                return customerVotedPollVoice.getVotedOptionCaption();
            }
        }
        return null;
    }

}
