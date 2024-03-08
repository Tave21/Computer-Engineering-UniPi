package it.unipi.dii.utility;
import java.util.List;
import java.util.Objects;

public class GenericUtility  {
    /**
     * @param inputList List of strings.
     * @param inputString The string to test.
     * @return true if str is in the list.
     */
    private boolean stringInList(List<String> inputList, String inputString) {
        if (inputList.isEmpty()) {
            return false;
        }
        for (String s : inputList) {
            if (Objects.equals(s, inputString)) {
                return true;
            }
        }
        return false;
    }
}