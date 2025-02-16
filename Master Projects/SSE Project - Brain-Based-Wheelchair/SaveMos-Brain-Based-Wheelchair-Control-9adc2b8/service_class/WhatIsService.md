The **ServiceClassOrchestrator** is designed to test and manage different phases of the data factory. Depending on the configuration, the orchestrator dynamically adjusts its behavior to execute specific tests for *development*, *production*, or all phases. Each test records timestamps and outcomes to facilitate performance analysis.

#### Key Features

1. **Dynamic Test Selection**:
   - The orchestrator determines the test to run based on a parameter (`phase`) from the `service_class_parameters.json` file. Possible values:
     - `"all_phases"`: Tests all phases: development, production, and evaluation.
     - `"development"`: Focuses exclusively on testing the development phase for multiple classifiers.
     - `"production"`: Only evaluates the production phase for an incremental number of sessions.

2. **Testing Scenarios**:
   - **All Phases Test**:
     - Simulates the full lifecycle of a single classifier: development → production → evaluation.
     - Sends records in random order (shuffled) to simulate real-world ingestion randomness.
     - Logs timestamps for each phase's start and end, including transitions between them.
     - Avoids sending *labels* (i.e., `movements`) during the production phase.
   - **Development Test**:
     - Tests multiple development phases for `classifiers_to_develop` classifiers (where `classifiers_to_develop` is a parameter).
     - For each classifier:
       - Initializes a bucket with session records and sends them randomly to the ingestion system.
       - Records timestamps for the start and end of each classifier's development.
       - Ends when a `production` configuration message is received.
     - Generates a log file to analyze the average development time per classifier.
   - **Production Test**:
     - Evaluates production for an incremental number of sessions (e.g., 1, 2, ..., `production_sessions` sessions).
     - For each step:
       - Prepares a bucket with `i` session records, sends them to the ingestion system, and waits for responses.
       - Logs the start and end timestamps to measure system response time for increasing workloads.

3. **Bucket Management**:
   - Records for a given phase are placed in a "bucket" (list) to allow randomized sending to the ingestion system.
   - Each record is removed from the bucket once sent, ensuring no duplication.

4. **Logging**:
   - Introduced a dedicated `CSVLogger` class to handle CSV file operations.
   - Ensures that new log files do not overwrite existing ones by appending an incremental number to filenames (e.g., `all_phases_log_0.csv`, `all_phases_log_1.csv`).
   - Logs are created with headers relevant to the phase being tested:
     - **Development Log**: `developed_classifier,timestamp,status`
     - **Production Log**: `sessions,timestamp,status`
     - **All Phases Log**: `phase,timestamp,status`

5. **Performance Metrics**:
   - Logs enable the creation of graphs to analyze:
     - Average development time per classifier (development phase test).
     - System response time for increasing session loads (production phase test).

### Logging Formats
1. **Development Phase**:
   ```csv
   developed_classifier,timestamp,status
   1,1616172675.123,start
   1,1616172680.456,records_sent
   1,1616172685.789,production
   ```

2. **Production Phase**:
   ```csv
   sessions,timestamp,status
   1,1616172675.123,start
   1,1616172680.456,records_sent
   1,1616172685.789,labels_received
   ```

3. **All Phases**:
   ```csv
   phase,timestamp,status
   development,1616172675.123,start
   development,1616172680.456,records_sent
   development,1616172685.789,production
   ```
