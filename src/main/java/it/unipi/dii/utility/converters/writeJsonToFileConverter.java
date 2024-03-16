package it.unipi.dii.utility.converters;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.util.List;

public class writeJsonToFileConverter {
    /**
     * Write a list of object as an array of object document in a local file.
     * @param objects List of the object to write.
     * @param filePath File in which the function have to write, if it does not exist it will be created, if it exists it will be overwritten.
     * @param <T> Generic.
     */
    public static <T> void writeToJsonFile(List<T> objects, String filePath) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.writeValue(new File(filePath), objects);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * @param filePath The file to delete, is it does not exist the function does nothing.
     */
    public static void deleteFile(String filePath) {
        if (!filePath.isEmpty()) {
            File file = new File(filePath);
            if (file.exists()) {
                if (file.delete()) {
                    System.out.println("'" + filePath + "' deleted successfully.");
                } else {
                    System.out.println("Failed to delete '" + filePath + "'");
                }
            } else {
                System.out.println("'" + filePath + "' does not exist.");
            }
        }
    }
}
