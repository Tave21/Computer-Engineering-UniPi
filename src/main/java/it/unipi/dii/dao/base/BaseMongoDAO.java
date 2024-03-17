package it.unipi.dii.dao.base;

import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.ReadPreference;
import com.mongodb.WriteConcern;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoDatabase;

public class BaseMongoDAO {
    public MongoDatabase mongoDB;

    protected MongoClient client; // It represents a pool of connections to the database, also for replicas.
    public static final String MONGO_PRIMARY_HOST = "localhost"; //10.1.1.55
    public static final Integer MONGO_PRIMARY_HOST_PORT = 27017;
    public static String MONGO_SECONDARY_HOST = "localhost";//"10.1.1.54";
    public static int MONGO_SECONDARY_HOST_PORT = 27019;
    public static String MONGO_THIRD_HOST = "localhost";//"10.1.1.48";
    public static int MONGO_THIRD_HOST_PORT = 27020;
    public static final String MONGO_DATABASE_NAME = "BeansBet";
    public static final String DB_URL = "mongodb://" + MONGO_PRIMARY_HOST + ":" + MONGO_PRIMARY_HOST_PORT;

    public BaseMongoDAO() {
        this.client = null;
    }

    public void closeConnection() {
        if (this.client != null) {
            this.client.close(); // Clean up resources at the end of the application.
        }
    }

    public void setClient(MongoClient client) {
        this.client = client;
    }

    public MongoClient getClient() {
        return this.client;
    }

    public void openConnection() {
        if (this.client == null) {
            try {
                ConnectionString uriString = new ConnectionString(DB_URL);
                MongoClientSettings mcs = MongoClientSettings.builder().applyConnectionString(uriString).readPreference(ReadPreference.primaryPreferred()).retryWrites(true).writeConcern(WriteConcern.W1).build();
                this.client = MongoClients.create(mcs);
                this.mongoDB = this.client.getDatabase(MONGO_DATABASE_NAME);
            } catch (Exception e) {
                System.out.println("problems with connection to mongodb");
            }
        }
    }

    public void openStrictConnection() {
        if (this.client == null) {
            try {
                ConnectionString uriString = new ConnectionString(DB_URL);
                MongoClientSettings mcs = MongoClientSettings.builder().applyConnectionString(uriString).readPreference(ReadPreference.primary()).retryWrites(true).writeConcern(WriteConcern.W1).build(); //W3 for replicas
                this.client = MongoClients.create(mcs);
                this.mongoDB = this.client.getDatabase(MONGO_DATABASE_NAME);
            } catch (Exception e) {
                System.out.println("problems with connection to mongodb");
            }
        }
    }

    public void openStrictConnectiona() {
        if (this.client == null) {
            // Client not connected.
            try {
                String url = "mongodb://" + MONGO_PRIMARY_HOST + ":" + MONGO_PRIMARY_HOST_PORT + "," + MONGO_SECONDARY_HOST + ":" + MONGO_SECONDARY_HOST_PORT + "," + MONGO_THIRD_HOST + ":" + MONGO_THIRD_HOST_PORT;
                ConnectionString uriString = new ConnectionString(url);
                MongoClientSettings mcs = MongoClientSettings.builder().applyConnectionString(uriString).readPreference(ReadPreference.primary()).retryWrites(true).writeConcern(WriteConcern.W1).build(); //W3 for replicas
                this.client = MongoClients.create(mcs);
                this.mongoDB = this.client.getDatabase(MONGO_DATABASE_NAME);
            } catch (Exception e) {
                System.out.println("Problems occurs with the connection to MongoDB!");
                e.printStackTrace();
            }

        }
    }

    public MongoClient openConnectiona() {
        if (this.client != null) {
            // Client already connected.
            return this.client;
        } else {
            // Client not connected.
            try {
                String url = "mongodb://" + MONGO_PRIMARY_HOST + ":" + MONGO_PRIMARY_HOST_PORT + "," + MONGO_SECONDARY_HOST + ":" + MONGO_SECONDARY_HOST_PORT + "," + MONGO_THIRD_HOST + ":" + MONGO_THIRD_HOST_PORT;
                ConnectionString uriString = new ConnectionString(url);
                MongoClientSettings mcs = MongoClientSettings.builder().applyConnectionString(uriString).readPreference(ReadPreference.primaryPreferred()).retryWrites(true).writeConcern(WriteConcern.W1).build();
                this.client = MongoClients.create(mcs);
                this.mongoDB = this.client.getDatabase(MONGO_DATABASE_NAME);

                System.out.println("Connection to MongoDB.");
                return client;
            } catch (Exception e) {
                System.out.println("Problems occurs with the connection to MongoDB!");
                e.printStackTrace();
                return null;
            }

        }
    }

}