module it.unipi.dii {
    requires javafx.controls;
    requires javafx.fxml;
    requires javafx.web;
    requires org.controlsfx.controls;
    requires com.dlsc.formsfx;
    requires net.synedra.validatorfx;
    requires org.kordamp.ikonli.javafx;
    requires org.kordamp.bootstrapfx.core;
    requires com.almasb.fxgl.all;
    requires org.mongodb.bson;
    requires com.fasterxml.jackson.annotation;
    requires org.mongodb.driver.core;
    requires org.mongodb.driver.sync.client;
    requires redis.clients.jedis;
    requires com.fasterxml.jackson.databind;
    requires logback.classic;
    requires slf4j.api;
    requires org.json;
    requires com.google.gson;
    requires com.opencsv;
    requires json.simple;
    requires junit;

    opens it.unipi.dii to javafx.fxml;
    exports it.unipi.dii;
    exports it.unipi.dii.pagesGUI;

    exports it.unipi.dii.model;
    exports it.unipi.dii.utility;
    exports it.unipi.dii.dao;
    exports it.unipi.dii.dto;
    exports it.unipi.dii.periodicUpdates;
    exports it.unipi.dii.dao.mongo;
    exports it.unipi.dii.dao.redis;
    exports it.unipi.dii.dao.base;

    opens it.unipi.dii.pagesGUI to javafx.fxml;
    exports it.unipi.dii.testing;
    opens it.unipi.dii.testing to javafx.fxml;
    exports it.unipi.dii.analyticsPeriodicCalculator;
    exports it.unipi.dii.utility.converters;
    exports it.unipi.dii.utility.generators;
    exports it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.financialResults;
    exports it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.seventhQuery;
    opens it.unipi.dii.model to javafx.fxml;
}