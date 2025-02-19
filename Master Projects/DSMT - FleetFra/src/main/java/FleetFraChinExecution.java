import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.entity.StringEntity;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class FleetFraChinExecution {

    /**
     * Utility method to send a POST request with a JSON payload.
     * @param url The target URL.
     * @param jsonPayload The JSON string to send as the request body.
     * @return The response as a String.
     * @throws Exception If an error occurs during the request.
     */
    public static String sendPostRequest(String url, String jsonPayload) throws Exception {
        HttpPost postRequest = new HttpPost(url);
        postRequest.setEntity(new StringEntity(jsonPayload, StandardCharsets.UTF_8));
        postRequest.setHeader("Content-Type", "application/json");

        try (CloseableHttpClient client = HttpClients.createDefault()) {
            HttpResponse response = client.execute(postRequest);
            BufferedReader reader = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));
            StringBuilder responseString = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                responseString.append(line);
            }
            return responseString.toString();
        }
    }

    /**
     * Creates a request to start a new game.
     * @param gameId The unique game identifier.
     * @param player1 The first player's ID.
     * @param player2 The second player's ID.
     * @return JSON formatted request string.
     * @throws Exception If an error occurs during JSON processing.
     */
    public static String createStartGameRequest(String gameId, String player1, String player2) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        return objectMapper.writeValueAsString(Map.of(
                "game_id", gameId,
                "type_request", "start_game",
                "player1", player1,
                "player2", player2,
                "player1_battlefield", generateBattlefield(),
                "player2_battlefield", generateBattlefield()
        ));
    }

    public static String createStartGameRequestClient(String gameId, String player , List<Map<String, Integer>> battle) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        return objectMapper.writeValueAsString(Map.of(
                "game_id", gameId,
                "type_request", "start_game_client",
                "player", player,
                "player_battlefield", battle
        ));
    }

    public static String getInfoRequest(String gameId) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        return objectMapper.writeValueAsString(Map.of(
                "game_id", gameId,
                "type_request", "get_game_info"
        ));
    }

    /**
     * Creates a request for making a move.
     * @param gameId The unique game identifier.
     * @param player The player's ID making the move.
     * @param row The row coordinate of the move.
     * @param col The column coordinate of the move.
     * @return JSON formatted request string.
     * @throws Exception If an error occurs during JSON processing.
     */
    public static String createMakeMoveRequest(String gameId, String player, int row, int col) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        return objectMapper.writeValueAsString(Map.of(
                "game_id", gameId,
                "type_request", "make_move",
                "player", player,
                "move", Map.of("row", row, "col", col)
        ));
    }

    private static final int ROWS = 11;
    private static final int COLS = ROWS;
    private static final int MIN_ROW = 1;
    private static final int MIN_COL = MIN_ROW;
    private static final int[] SHIP_LENGTHS = {3, 4, 5};
    private static final Random RANDOM = new Random();

    public static List<Map<String, Integer>> generateBattlefield() {
        List<Map<String, Integer>> battlefield = new ArrayList<>();
        int[][] grid = new int[ROWS][COLS];

        // Initialize battlefield with empty cells
        for (int row = MIN_ROW; row < ROWS; row++) {
            for (int col = MIN_COL; col < COLS; col++) {
                battlefield.add(new HashMap<>(Map.of("row", row, "col", col, "value", 0)));
            }
        }

        // Place ships randomly
        for (int length : SHIP_LENGTHS) {
            placeShipRandomly(grid, length);
        }

        // Update battlefield based on grid
        for (int row = MIN_ROW; row < ROWS; row++) {
            for (int col = MIN_COL; col < COLS; col++) {
                if (grid[row][col] == 1) {
                    setCell(battlefield, row, col);
                }
            }
        }

        return battlefield;
    }

    public static List<Map<String, Integer>> generateBattlefield(boolean zero) {
        List<Map<String, Integer>> battlefield = new ArrayList<>();
        int[][] grid = new int[ROWS][COLS];

        // Initialize battlefield with empty cells
        for (int row = MIN_ROW; row < ROWS; row++) {
            for (int col = MIN_COL; col < COLS; col++) {
                battlefield.add(new HashMap<>(Map.of("row", row, "col", col, "value", 0)));
            }
        }

        // Place ships randomly
        for (int length : SHIP_LENGTHS) {
            placeShipRandomly(grid, length);
        }

        // Update battlefield based on grid
        for (int row = MIN_ROW; row < ROWS; row++) {
            for (int col = MIN_COL; col < COLS; col++) {
                if (grid[row][col] == 1 && !zero) {
                    setCell(battlefield, row, col);
                }
            }
        }

        return battlefield;
    }

    private static void placeShipRandomly(int[][] grid, int length) {
        boolean placed = false;

        while (!placed) {
            int row = RANDOM.nextInt(ROWS);
            int col = RANDOM.nextInt(COLS);
            boolean horizontal = RANDOM.nextBoolean();

            if (canPlaceShip(grid, row, col, length, horizontal)) {
                for (int i = 0; i < length; i++) {
                    if (horizontal) {
                        grid[row][col + i] = 1;
                    } else {
                        grid[row + i][col] = 1;
                    }
                }
                placed = true;
            }
        }
    }

    private static boolean canPlaceShip(int[][] grid, int row, int col, int length, boolean horizontal) {
        if (horizontal) {
            if (col + length > COLS) return false;
            for (int i = 0; i < length; i++) {
                if (grid[row][col + i] == 1) return false;
            }
        } else {
            if (row + length > ROWS) return false;
            for (int i = 0; i < length; i++) {
                if (grid[row + i][col] == 1) return false;
            }
        }
        return true;
    }

    private static void setCell(List<Map<String, Integer>> battlefield, int row, int col) {
        for (Map<String, Integer> cell : battlefield) {
            if (cell.get("row") == row && cell.get("col") == col) {
                cell.put("value", 1);
                break;
            }
        }
    }

    private static final String CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    public static String generateRandomString(int length) {
        Random random = new Random();
        StringBuilder result = new StringBuilder();

        for (int i = 0; i < length; i++) {
            int randomIndex = random.nextInt(CHARACTERS.length());
            result.append(CHARACTERS.charAt(randomIndex));
        }

        return result.toString();
    }

}
