package it.unipi.dii.dao.mongo;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import it.unipi.dii.dao.CustomerDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import it.unipi.dii.model.Customer;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static it.unipi.dii.utility.JsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.MongoUtility.deleteDocuments;
import static it.unipi.dii.utility.MongoUtility.insertDocuments;
import static it.unipi.dii.utility.ObjectToDocument.ObjectToDocumentConverter;
import static it.unipi.dii.utility.ObjectToJsonString.convertObjectToJsonString;
import static it.unipi.dii.utility.RandomNumber.truncateNumber;
import static it.unipi.dii.utility.Security.CheckHash;
import static java.lang.Math.abs;

public class CustomerMongoDBDAO extends BaseMongoDAO implements CustomerDAO {
    /**
     *
     * @param user The customer to register.
     * @return True if the registration has been successfully done.
     */
    @Override
    public boolean registerCustomer(Customer user) {
        if (!customerInfoAlreadyPresent(user)) {
            return false; // Info already present in the system.
        } else {
            user.setCredit(0);
            List<Document> documents = new ArrayList<>();
            Document doc = Document.parse(convertObjectToJsonString(user));
            documents.add(doc);
            MongoCollection<Document> user_coll = this.mongoDB.getCollection("customers");
            insertDocuments(user_coll, documents);
            return true;
        }
    }

    /**
     * @param username Username of the customer.
     * @param password Password of the customer.
     * @return True if the login has been successfully done.
     */
    @Override
    public Customer authenticateCustomer(String username, String password) {
        Customer c = fetchCustomerInformation(username);
        if (c == null) {
            return null; // The user does not exist.
        } else {
            if (CheckHash(password, c.getPassword())) {
                return c; // Correct password.
            } else {
                return null;
            }
        }
    }

    /**
     *
     * @param username of the customer.
     * @return The related customer object.
     */
    @Override
    public Customer fetchCustomerInformation(String username) {
        List<Customer> ml = getCustomers(
                new Document("username", username),
                new Document("_id", 0)
        );
        if (ml.isEmpty()) {
            return null;
        } else {
            return ml.get(0);
        }
    }

    /**
     * @param user The customer object to test.
     * @return True if there is a customer with the same attributes below in the database.
     * These attributes must be unique in the collection customers.
     * - username
     * - email
     * - cellNumber
     */

    @Override
    public boolean customerInfoAlreadyPresent(Customer user) {
        List<Customer> lc = getCustomers(
                new Document("username", user.getUsername()),
                new Document("_id", 0)
        );
        return lc.isEmpty();
    }

    /**
     * Remove all the customers that match the criteria.
     * @param query Match criteria.
     */

    @Override
    public void removeCustomer(Document query) {
        MongoCollection<Document> customer_coll = this.mongoDB.getCollection("customers");
        deleteDocuments(customer_coll, query);
        
    }

    /**
     * Remove the customer with the target username from the database.
     * @param username The username of the customer to remove from the database.
     */
    @Override
    public void removeCustomer(String username) {
        Document query = new Document("username", new Document("$eq", username));
        System.out.println(query.toString());
        removeCustomer(query);
    }

    /**
     *
     * @param query Match criteria.
     * @param newCustomer The new customer information.
     */

    @Override
    public void replaceCustomer(Document query, Customer newCustomer) {
        MongoCollection<Document> match_coll = this.mongoDB.getCollection("customers");
        match_coll.replaceOne(query, ObjectToDocumentConverter(newCustomer));
        
    }
    /**
     * Replace the customer with the same username of newCustomer.
     * @param newCustomer The new customer information.
     */
    @Override
    public void replaceCustomer(Customer newCustomer) {
        replaceCustomer(new Document("username", new Document("$eq", newCustomer.getUsername())), newCustomer);
    }

    /**
     *
     * @param query Match criteria.
     * @param projection Projection criteria.
     * @return The customer list that match the criteria.
     */
    @Override
    public List<Customer> getCustomers(Document query, Document projection) {
        MongoCollection<Document> customers_coll = this.mongoDB.getCollection("customers");
        List<Customer> s_list = new ArrayList<>();

        if(query == null){
            try (MongoCursor<Document> cursor = customers_coll.find().projection(projection).iterator()) {
                while (cursor.hasNext()) {
                    Document document = cursor.next();
                    Customer s = convertJsonToObject(document.toJson(), Customer.class);
                    s_list.add(s);
                }
            }
        }else {
            try (MongoCursor<Document> cursor = customers_coll.find(query).projection(projection).iterator()) {
                while (cursor.hasNext()) {
                    Document document = cursor.next();
                    Customer s = convertJsonToObject(document.toJson(), Customer.class);
                    s_list.add(s);
                }
            }
        }

        return s_list;
    }

    /**
     * Add credit to a user in the database.
     * @param Username The username of the customer.
     * @param howMuch How much credit it has to be added.
     */

    @Override
    public void redeem(String Username, double howMuch) {
        howMuch = truncateNumber(abs(howMuch),2);
        if (howMuch > 0) {
            Document query = new Document("username", new Document("$eq", Username));
            Document update = new Document("$inc", new Document("credit", howMuch));
            updateCustomer(query,update);

        }
    }
    /**
     * Remove credit to a user in the MongoDB database.
     * @param Username The username of the customer.
     * @param howMuch How much credit it has to be removed; if a negative number is given, the absolute value will be used.
     */
    @Override
    public boolean pay(String Username, double howMuch) {
        howMuch = truncateNumber(abs(howMuch) , 2); // Clear the input.
        if (howMuch != 0) {
            double cred = getCreditOfCustomer(Username);
            if (cred == -1) {
                return false; // The user does not exist.
            } else {
                if(cred >= howMuch) {
                    Document query = new Document("username", Username);
                    Document update = new Document("$inc", new Document("credit", -1 * howMuch));
                    updateCustomer(query, update);
                    return true;
                }else{
                    return false; // The customer does not have enough money
                }
            }
        }
        return true; // Payment successfully done.
    }

    /**
     * Apply the update specification to all the customer document that respect the criteria.
     * @param query Match criteria.
     * @param update Update specification.
     */

    @Override
    public void updateCustomer(Document query, Document update) {
        MongoCollection<Document> customers_coll = this.mongoDB.getCollection("customers");
        customers_coll.updateOne(query, update);
        
    }

    /**
     * Retrieve from the database the credit of the target user.
     * @param username The target customer's username.
     * @return The credit of the user.
     */
    @Override
    public double getCreditOfCustomer(String username) {
        List<Document> pipeline = Arrays.asList(new Document("$match",
                        new Document("username", username)),
                new Document("$project",
                        new Document("credit", "$credit")
                                .append("_id", 0L)),
                new Document("$limit", 1L));
        AggregateIterable<Document> docs = this.mongoDB.getCollection("customers").aggregate(pipeline);
        try {
            return Objects.requireNonNull(docs.first()).getDouble("credit");
        }catch(NullPointerException e){
            return -1;
        }
    }
}
