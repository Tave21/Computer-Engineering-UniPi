package it.unipi.dii.utility;


import java.io.FileInputStream;
import java.util.Properties;

public class utilclass  {

    public static Properties readConfigurationParameters(){
        try{
            FileInputStream fileInputStream=new FileInputStream(utilclass.class.getResource("/config/config.properties").toURI().getPath());
            Properties properties=new Properties();
            properties.load(fileInputStream);
            return properties;
        }catch (Exception e){
            System.out.println("problems with property file reading");
            e.printStackTrace();
        }
        return null;
    }
}