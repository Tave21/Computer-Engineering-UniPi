package it.unipi.dii.utility.converters;

import org.bson.Document;

public class jsonToDocumentConverter {
    /**
     * @param Json JSON String to convert
     * @return The document equivalent of the JSON String.
     */
    public static Document convertJsonToDocument(String Json){
        return Document.parse(Json);
    }
    /**
     * @param doc Document to convert
     * @return The JSON equivalent of the document doc.
     */
    public static String convertDocumentToJson(Document doc){
        return doc.toJson();
    }
}
