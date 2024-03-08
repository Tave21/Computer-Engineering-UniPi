package it.unipi.dii.utility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.bson.Document;
import java.util.List;

public class JsonToObjectConverter {
    /**
     *
     * @param jsonString JSON to convert
     * @param objectType target object type (specify with "Object.class")
     * @return An object of the specified type or null if something goes wrong.
     * @param <T> Generic type.
     */
    public static <T> T convertJsonToObject(String jsonString, Class<T> objectType) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            return objectMapper.convertValue(Document.parse(jsonString), objectType);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}