package it.unipi.dii.utility;

import org.bson.Document;

import static it.unipi.dii.utility.JsonToDocument.convertJsonToDocument;
import static it.unipi.dii.utility.ObjectToJsonString.convertObjectToJsonString;

public class ObjectToDocument {
    /**
     * This function convert an object to the Document version of it.
     * @param object The object to convert.
     * @return The document-version of the object.
     * @param <T> Generic.
     */
    public static <T> Document ObjectToDocumentConverter(T object){
        return convertJsonToDocument(convertObjectToJsonString(object));
    }
}
