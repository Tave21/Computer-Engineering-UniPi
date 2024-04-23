package it.unipi.dii.model;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Objects;

public class Multiplier {
    private String name;
    private double value;
    private final static double MIN_VALUE = 1.1;

    /**
     * Multiplier Format: {"1X" : 1.69}
     * @param name The multiplier name.
     * @param value The multiplier value.
     */
    public Multiplier(String name, double value) {
        this.name = name;
        this.value = value;
    }

    public Multiplier(){}

    @JsonProperty("name")
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    @JsonProperty("value")
    public double getValue() {
        return value;
    }
    public void setValue(double value) {
        this.value = value;
    }

    public boolean validMultiplier(){
        return this.value >= MIN_VALUE && !Objects.equals(this.name, "-");
    }
}
