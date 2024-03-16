package it.unipi.dii.dao.mongo;

import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import it.unipi.dii.dao.AdminDAO;
import it.unipi.dii.dao.base.BaseMongoDAO;
import it.unipi.dii.model.Admin;

import org.bson.Document;

import java.util.ArrayList;
import java.util.List;

import static it.unipi.dii.utility.converters.jsonToObjectConverter.convertJsonToObject;
import static it.unipi.dii.utility.securityLibrary.CheckHash;

public class AdminMongoDBDAO extends BaseMongoDAO implements AdminDAO {

    /**
     * @param email email of the admin.
     * @param password Password of the admin.
     * @return True if the login has been successfully done.
     */
    @Override
    public Admin authenticateAdmin(String email, String password) {
        Admin c = fetchAdminInformation(email);
        if(c == null){
            return null; // The admin does not exist.
        }else{
            if(CheckHash(password , c.getPassword())){
                return c; // Correct password.
            }else{
                return null;
            }
        }
    }

    /**
     *
     * @param email of the admin.
     * @return The Admin object.
     */

    @Override
    public Admin fetchAdminInformation(String email) {
        List<Admin> ml = getAdmins(new Document("email", email), new Document("_id", 0));
        if (ml.isEmpty()) {
            return null;
        } else {
            return ml.get(0);
        }
    }

    /**
     *
     * @param query Match criteria.
     * @param projection Projection criteria.
     * @return The admin list that match the criteria.
     */

    @Override
    public List<Admin> getAdmins(Document query, Document projection) {
        MongoCollection<Document> Admins_coll = this.mongoDB.getCollection("admins");
        List<Admin> s_list = new ArrayList<>();
        try (MongoCursor<Document> cursor = Admins_coll.find(query).projection(projection).iterator()) {
            while (cursor.hasNext()) {
                Document document = cursor.next();
                Admin s = convertJsonToObject(document.toJson() , Admin.class);
                s_list.add(s);
            }
        }

        return s_list;
    }
}
