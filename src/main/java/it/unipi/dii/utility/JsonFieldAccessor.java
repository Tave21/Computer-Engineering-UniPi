package it.unipi.dii.utility;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonFieldAccessor {
    /**
     * @param jsonString String that contains a JSON object.
     * @param fieldName Tag of the field-value to get.
     * @return The value of the field at the specified tag.
     */
    public static String getJsonValue(String jsonString, String fieldName) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            JsonNode jsonNode = objectMapper.readTree(jsonString);
            if (jsonNode.has(fieldName)) {
                // Check if the specified field exists
                return jsonNode.get(fieldName).asText(); // Access the field and return its value as a string
            } else {
                System.out.println("Field '" + fieldName + "' not found in the JSON.");
                return null;
            }
        } catch (Exception e) {
            return null;
        }
    }
    /**
     * @param jsonString String that contains a JSON object.
     * @param index Index of the field-value to get.
     * @return The value of the field at the specified index.
     */
    public static double getJsonValueIndex(String jsonString, int index) {
        ObjectMapper objectMapper = new ObjectMapper();

        try {
            JsonNode multipliersArray = objectMapper.readTree(jsonString).get("multipliers");
            if (multipliersArray.isArray() && multipliersArray.size() >= (index + 1)) {
                JsonNode element = multipliersArray.get(index);
                return element.get("value").asDouble();
            }
            return -1;
        } catch (Exception e) {
            return -1;
        }
    }

    /**
     * @param jsonString String that contains a JSON object.
     * @param index Index of the field-tag to get.
     * @return The tag name of the field at the specified index.
     */
    public static String getJsonTagIndex(String jsonString, int index) {
        ObjectMapper objectMapper = new ObjectMapper();
        try {
            JsonNode multipliersArray = objectMapper.readTree(jsonString).get("multipliers");
            if (multipliersArray.isArray() && multipliersArray.size() >= (index + 1)) {
                return multipliersArray.get(index).get("tag").asText();
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }


}
