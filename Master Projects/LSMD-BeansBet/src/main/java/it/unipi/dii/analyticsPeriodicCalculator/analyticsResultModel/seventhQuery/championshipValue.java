package it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.seventhQuery;


import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;

public class championshipValue {

    private String championship_id;
    private double value;

    public championshipValue(String championship_id, double value) {
        this.championship_id = championship_id;
        this.value = value;
    }

    public championshipValue() {
    }

    @JsonProperty("championship_id")
    public String getChampionship_id() {
        return championship_id;
    }


    @JsonProperty("value")
    public double getValue() {
        return value;
    }

    public void setChampionship_id(String championship_id) {
        this.championship_id = championship_id;
    }

    public void setValue(double value) {
        this.value = value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(championship_id, value);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        championshipValue that = (championshipValue) obj;
        return Double.compare(that.value, value) == 0 &&
                Objects.equals(championship_id, that.championship_id);
    }

    @Override
    public String toString() {
        return "{" +
                "championship_id:'" + championship_id + '\'' +
                ", value:" + value +
                '}';
    }

}