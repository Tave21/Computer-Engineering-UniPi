package it.unipi.dii.utility;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.util.List;

public class ListToJsonConverter {
    /**
     * Create a JSON file and write the content of the list in JSON format.
     * @param objects Is a List of objects.
     * @param filePath Path of the file where export the list.
     */
    public static <T> void writeToJsonFile(List<T> objects, String filePath) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.writeValue(new File(filePath), objects);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
