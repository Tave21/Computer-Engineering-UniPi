package it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.financialResults;
import com.fasterxml.jackson.annotation.JsonProperty;
import it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.seventhQuery.championshipValue;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class financialReport {
    private String type;
    private String periodRelated; // The period related to this report.
    private String computationTimestamp; // The timestamp when this computation has been done.
    private List<financialValue> valueList;

    public financialReport(String periodRelated, String computationTimestamp, List<financialValue> valueList) {
        this.periodRelated = periodRelated;
        this.computationTimestamp = computationTimestamp;
        this.valueList = valueList;
    }

    public financialReport() {
        this.valueList = new ArrayList<>();
    }

    @JsonProperty("type")
    public String getType() {
        return this.type;
    }
    @JsonProperty("periodRelated")
    public String getPeriodRelated() {
        return this.periodRelated;
    }

    @JsonProperty("computationTimestamp")
    public String getComputationTimestamp() {
        return this.computationTimestamp;
    }

    @JsonProperty("valueList")
    public List<financialValue> getValueList() {
        return this.valueList;
    }

    public void setPeriodRelated(String periodRelated) {
        this.periodRelated = periodRelated;
    }

    public void setComputationTimestamp(String computationTimestamp) {
        this.computationTimestamp = computationTimestamp;
    }
    public void setType(String type) {
        this.type = type;
    }

    public void setValueList(List<financialValue> valueList) {
        this.valueList = valueList;
    }

    /**
     * Add a new financial value to the list.
     *
     * @param f The financial value to add.
     */

    public void addValueToList(financialValue f) {
        this.valueList.add(f);
    }


    @Override
    public int hashCode() {
        return Objects.hash(type, periodRelated, computationTimestamp, valueList);
    }

    @Override
    public String toString() {
        return "FinancialValue{" +
                "type='" + type + '\'' +
                ", periodRelated='" + periodRelated + '\'' +
                ", computationTimestamp='" + computationTimestamp + '\'' +
                ", valueList=" + valueList +
                '}';
    }

}
