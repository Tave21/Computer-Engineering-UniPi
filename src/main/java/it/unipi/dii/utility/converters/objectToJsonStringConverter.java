package it.unipi.dii.utility.converters;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class objectToJsonStringConverter {
    /**
     * @param object The object to convert.
     * @return A String which contains the JSON version of the object, or NULL if something goes wrong.
     * @param <T> Generic.
     */
    public static <T> String convertObjectToJsonString(T object) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            return objectMapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
            return null;
        }
    }

}