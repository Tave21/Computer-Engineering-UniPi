import static org.junit.jupiter.api.Assertions.*;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.*;
import org.springframework.boot.test.context.SpringBootTest;

import java.net.URI;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * WebSocket test suite for FleetFraChin.
 * This test suite verifies WebSocket communication, game initialization, move validation,
 * turn handling, and error responses.
 */
@SpringBootTest(classes = FleetFraChinExecution.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TestFleetFraChinWebSocket {

    // Constants for game and player identifiers
    private static final String GAME_ID = FleetFraChinExecution.generateRandomString(20);
    private static final String PLAYER1_ID = "player111";
    private static final String PLAYER2_ID = "player222";
    private static final String SERVER_URL = "ws://10.2.1.27:8080/ws";

    private static WebSocketClient client1;
    private static WebSocketClient client2;
    private static CountDownLatch latch;
    private static String receivedMessage;
    private static String receivedMessage1;
    private static String receivedMessage2;

    /**
     * Initializes a WebSocket client before each test.
     * Establishes a connection and ensures WebSocket is open.
     */
    @BeforeAll
    public static void setup() throws Exception {
        latch = new CountDownLatch(1);
        receivedMessage = null;
        receivedMessage1 = null;
        receivedMessage2 = null;

        client1 = new WebSocketClient(new URI(SERVER_URL + "?GameID=" + GAME_ID)) {
            @Override
            public void onOpen(ServerHandshake handshake) {
                System.out.println("✅ Client1 - WebSocket connected");
            }

            @Override
            public void onMessage(String message) {
                System.out.println("📩 Client1 - Received: " + message);
                receivedMessage1 = message; //.replace("\\\"", "\""); // Normalize JSON format
                latch.countDown();
            }

            @Override
            public void onClose(int code, String reason, boolean remote) {
                System.out.println("❌ Client1 - WebSocket closed. Reason: " + reason);
            }

            @Override
            public void onError(Exception ex) {
                System.err.println("⚠️ Client1 - WebSocket error: " + ex.getMessage());
            }
        };

        client1.connectBlocking();
        assertTrue(client1.isOpen(), " Client1 - WebSocket connection is not active!");

        client2 = new WebSocketClient(new URI(SERVER_URL + "?GameID=" + GAME_ID)) {
            @Override
            public void onOpen(ServerHandshake handshake) {
                System.out.println("✅ Client2 - WebSocket connected");
            }

            @Override
            public void onMessage(String message) {
                System.out.println("📩 Client2 - Received: " + message);
                receivedMessage2 = message; //.replace("\\\"", "\""); // Normalize JSON format
                latch.countDown();
            }

            @Override
            public void onClose(int code, String reason, boolean remote) {
                System.out.println("❌ Client2 - WebSocket closed. Reason: " + reason);
            }

            @Override
            public void onError(Exception ex) {
                System.err.println("⚠️ Client2 - WebSocket error: " + ex.getMessage());
            }
        };

        client2.connectBlocking();
        assertTrue(client2.isOpen(), " Client2 - WebSocket connection is not active!");
    }

    /**
     * Sends a request and waits for the expected response.
     *
     * @param requestJson      The JSON request payload.
     * @param expectedResponse The expected JSON response.
     * @throws Exception if there is a timeout or assertion failure.
     */
    private String sendAndAwaitResponse(String requestJson, String expectedResponse,WebSocketClient client, int num) throws Exception {
        latch = new CountDownLatch(1); // Reset the latch for each request
        client.send(requestJson);
        boolean messageReceived = latch.await(10, TimeUnit.SECONDS); // Wait max 10s

        assertTrue(messageReceived, "❌ No response from server!");

        if (num == 1){
            assertNotNull(receivedMessage1, "❌ No message received!");
            assertEquals(expectedResponse, receivedMessage1);
            return receivedMessage1;
        }else{
            assertNotNull(receivedMessage2, "❌ No message received!");
            assertEquals(expectedResponse, receivedMessage2);
            return receivedMessage2;
        }
    }

    private String sendAndAwaitResponse(String requestJson,WebSocketClient client, int num) throws Exception {
        latch = new CountDownLatch(1); // Reset the latch for each request
        client.send(requestJson);
        boolean messageReceived = latch.await(10, TimeUnit.SECONDS); // Wait max 10s
        if (messageReceived) {
            if (num == 1){
                return receivedMessage1;
            }else{
                return receivedMessage2;
            }
        }else{
            return null;
        }

    }

    /**
     * Test to start a game and verify a successful response.
     */
    @Test
    @Order(1)
    void testStartGame() throws Exception {
        List<Map<String, Integer>> player1Battlefield = FleetFraChinExecution.generateBattlefield();
        sendAndAwaitResponse(
                FleetFraChinExecution.createStartGameRequestClient(GAME_ID, PLAYER1_ID ,player1Battlefield),
                "{\"message\":\"OK: Game started\"}", client1 , 1
        );

        //wait(1000);

        List<Map<String, Integer>> player2Battlefield = FleetFraChinExecution.generateBattlefield();
        sendAndAwaitResponse(
                FleetFraChinExecution.createStartGameRequestClient(GAME_ID, PLAYER2_ID ,player2Battlefield),
                "{\"message\":\"OK: Game started\"}", client2, 2
        );
    }

    /**
     * Test invalid move coordinates and expect an "Out of bound coordinates" response.
     */
    @Test
    @Order(2)
    void testInvalidMoves() throws Exception {
        String expectedResponse = "{\"message\":\"INVALID MOVE: Out of bound coordinates\"}";
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 0), expectedResponse, client1 , 1);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 1, -1), expectedResponse, client1, 1);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, -1, 1), expectedResponse, client1, 1);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 10, 0), expectedResponse, client1, 1);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 10), expectedResponse, client1, 1);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 11, 11), expectedResponse, client1, 1);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, -1, -1), expectedResponse, client1, 1);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 11, -1), expectedResponse, client1, 1);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, -1, 11), expectedResponse, client1, 1);
    }

    /**
     * Test turn validation error by attempting to move when it's not the player's turn.
     */
    @Test
    @Order(3)
    void testTurnError() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER2_ID, 1, 1),
                "{\"message\":\"TURN ERROR: Not your turn\"}", client2, 2
        );
    }

    /**
     * Test valid moves for both players and expect success responses.
     */
    @Test
    @Order(4)
    void testValidMove() throws Exception {
        String resp = null;
        WebSocketClient temp;
        int curr_num = 1;
        int wait_num = 2;
        int temp_num;
        Random random = new Random();
        int randomX, randomY, i;

        for (i = 0; i < 12; i++) {
            randomX = random.nextInt(10) +1;
            randomY = random.nextInt(10) +1;

            if(curr_num == 1)
                resp = sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, randomX, randomY), client1,   1);
            else
                resp = sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER2_ID, randomX, randomY), client2,   2);

            assert resp != null;

            if (resp.compareTo("{\"message\":\"OK: Move accepted [3]\"}") == 0){
                // Water hit
                temp_num = wait_num;
                wait_num = curr_num;
                curr_num = temp_num;
            } else if (resp.compareTo("{\"message\":\"OK: Move accepted [2]\"}") == 0) {
                continue;
            } else{
                fail();
            }

        }
    }


    /**
     * Test an invalid player attempting to make a move.
     */
    @Test
    @Order(5)
    void testInvalidPlayer() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest(GAME_ID, "fake_player", 1, 1),
                "{\"message\":\"ERROR: Player not found\"}", client1, 1
        );

        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest(GAME_ID, "fake_player", 1, 1),
                "{\"message\":\"ERROR: Player not found\"}", client2, 2
        );
    }

    /**
     * Test an invalid game identifier and expect an error response.
     */
    @Test
    @Order(6)
    void testInvalidGame() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest("fake_game", PLAYER1_ID, 1, 1),
                "{\"message\":\"ERROR: Game not found\"}", client1, 1
        );

        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest("fake_game", PLAYER2_ID, 1, 1),
                "{\"message\":\"ERROR: Game not found\"}", client2, 2
        );
    }


    /**
     * Test an entire game.
     */
    @Test
    @Order(7)
    void testFullGame() throws Exception {
        String gameID = FleetFraChinExecution.generateRandomString(20);
        String player1FinalState = "";
        String player2FinalState = "";
        String currentPlayer, otherPlayer;
        String requestJson, responseJson;
        int curr_num, wait_num ,temp_num;


        client1 = new WebSocketClient(new URI(SERVER_URL + "?GameID=" + gameID)) {
            @Override
            public void onOpen(ServerHandshake handshake) {
                System.out.println("✅ Client1 - WebSocket connected");
            }

            @Override
            public void onMessage(String message) {
                System.out.println("📩 Client1 - Received: " + message);
                receivedMessage1 = message.replace("\\\"", "\""); // Normalize JSON format
                latch.countDown();
            }

            @Override
            public void onClose(int code, String reason, boolean remote) {
                System.out.println("❌ Client1 - WebSocket closed. Reason: " + reason);
            }

            @Override
            public void onError(Exception ex) {
                System.err.println("⚠️ Client1 - WebSocket error: " + ex.getMessage());
            }
        };

        client1.connectBlocking();
        assertTrue(client1.isOpen(), " Client1 - WebSocket connection is not active!");

        client2 = new WebSocketClient(new URI(SERVER_URL + "?GameID=" + gameID)) {
            @Override
            public void onOpen(ServerHandshake handshake) {
                System.out.println("✅ Client2 - WebSocket connected");
            }

            @Override
            public void onMessage(String message) {
                System.out.println("📩 Client2 - Received: " + message);
                receivedMessage2 = message.replace("\\\"", "\""); // Normalize JSON format
                latch.countDown();
            }

            @Override
            public void onClose(int code, String reason, boolean remote) {
                System.out.println("❌ Client2 - WebSocket closed. Reason: " + reason);
            }

            @Override
            public void onError(Exception ex) {
                System.err.println("⚠️ Client2 - WebSocket error: " + ex.getMessage());
            }
        };

        client2.connectBlocking();
        assertTrue(client2.isOpen(), " Client2 - WebSocket connection is not active!");


        Random rand = new Random();
        List<Map<String, Integer>> player1Battlefield = FleetFraChinExecution.generateBattlefield();
        List<Map<String, Integer>> player2Battlefield = FleetFraChinExecution.generateBattlefield();

        // STARTING GAME TEST
        sendAndAwaitResponse(
                FleetFraChinExecution.createStartGameRequestClient(gameID, PLAYER2_ID ,player2Battlefield),
                "{\"message\":\"OK: Game started\"}",client2,2
        );

        sendAndAwaitResponse(
                FleetFraChinExecution.createStartGameRequestClient(gameID, PLAYER1_ID ,player1Battlefield),
                "{\"message\":\"OK: Game started\"}",client1,1
        );


        // List the ship positions for player2 (player1 has to sink them)
        List<Map<String, Integer>> player2ShipPositions = new ArrayList<>();
        for (Map<String, Integer> cell : player2Battlefield) {
            if (cell.get("value") == 1) {
                player2ShipPositions.add(cell);  // Collect the cells where player2's ships are located
            }
        }

        currentPlayer = PLAYER2_ID;
        otherPlayer = PLAYER1_ID;
        curr_num = 2;
        wait_num = 1;

        while (true) {
            int row, col;
            if (currentPlayer.equals(PLAYER1_ID) && !player2ShipPositions.isEmpty()) {
                // Player1 hits positions of player2's ships without missing
                Map<String, Integer> targetCell = player2ShipPositions.getFirst();
                row = targetCell.get("row");
                col = targetCell.get("col");

                // Remove the hit ship from the list
                player2ShipPositions.removeFirst();
            } else {
                // Player2 shoots randomly
                row = rand.nextInt(10) +1;
                col = rand.nextInt(10) +1;
            }

            requestJson = FleetFraChinExecution.createMakeMoveRequest(gameID, currentPlayer, row, col);

            if(curr_num == 1)
                responseJson = sendAndAwaitResponse(requestJson, client1,   1);
            else
                responseJson = sendAndAwaitResponse(requestJson, client2,   2);

            switch (responseJson) {
                case "{\"message\":\"OK: Move accepted [3]\"}" -> {
                    String temp = currentPlayer;
                    currentPlayer = otherPlayer;
                    otherPlayer = temp;

                    temp_num = wait_num;
                    wait_num = curr_num;
                    curr_num = temp_num;

                    System.out.println(currentPlayer + " has played.");
                }

                case "{\"message\":\"OK: Move accepted [2]\"}" -> {
                    System.out.println(currentPlayer + " has played.");
                }

                case "{\"message\":\"TURN ERROR: Not your turn\"}" -> {
                    System.out.println(currentPlayer + " not his turn.");
                }

                // Check if the game has ended (e.g., "VICTORY" or "DEFEAT" directly from the response)
                case "{\"message\":\"VICTORY\"}" -> {
                    if (currentPlayer.equals(PLAYER1_ID)) {
                        player1FinalState = "VICTORY";  // Player1 won
                        System.out.println(currentPlayer + " won.");
                    } else {
                        player2FinalState = "VICTORY";  // Player2 won
                        System.out.println(currentPlayer + " won.");
                    }

                    String temp = currentPlayer;
                    currentPlayer = otherPlayer;
                    otherPlayer = temp;

                    temp_num = wait_num;
                    wait_num = curr_num;
                    curr_num = temp_num;
                }
                case "{\"message\":\"DEFEAT\"}" -> {
                    if (currentPlayer.equals(PLAYER1_ID)) {
                        player1FinalState = "DEFEAT";  // Player1 lost
                        System.out.println(currentPlayer + " lost.");
                    } else {
                        player2FinalState = "DEFEAT";  // Player2 lost
                        System.out.println(currentPlayer + " lost.");
                    }
                    String temp = currentPlayer;
                    currentPlayer = otherPlayer;
                    otherPlayer = temp;

                    temp_num = wait_num;
                    wait_num = curr_num;
                    curr_num = temp_num;
                }
                case null -> throw new IllegalStateException("Null value");
                default -> throw new IllegalStateException("Unexpected value: " + responseJson);
            }

            if ((player1FinalState.equals("VICTORY") || player1FinalState.equals("DEFEAT")) &&
                    (player2FinalState.equals("VICTORY") || player2FinalState.equals("DEFEAT"))){
                break;  // End the game if both players have a final state (either victory or defeat)
            }
        }
    }

    /**
     * Cleanup method executed after each test.
     * Ensures WebSocket connection is closed properly.
     */
    @AfterAll
    public static void cleanup() {
        if (client1 != null && client1.isOpen()) {
            client1.close();
        }

        if (client2 != null && client2.isOpen()) {
            client2.close();
        }
    }
}
