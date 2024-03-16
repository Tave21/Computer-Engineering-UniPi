package it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.seventhQuery;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class mainReport {
    private String type;
    private String periodRelated;
    private String computationTimestamp;
    private List<championshipValue> valueList;

    public mainReport(String periodRelated, String computationTimestamp, List<championshipValue> valueList) {
        this.periodRelated = periodRelated;
        this.computationTimestamp = computationTimestamp;
        this.valueList = valueList;
    }

    public mainReport(){
        this.valueList = new ArrayList<>();
    }
    @JsonProperty("type")
    public String getType() {
        return this.type;
    }

    @JsonProperty("periodRelated")
    public String getPeriodRelated() {
        return periodRelated;
    }

    @JsonProperty("computationTimestamp")
    public String getComputationTimestamp() {
        return computationTimestamp;
    }

    @JsonProperty("valueList")
    public List<championshipValue> getValueList() {
        return valueList;
    }

    public void setPeriodRelated(String periodRelated) {
        this.periodRelated = periodRelated;
    }

    public void setType(String type) {
        this.type = type;
    }
    public void setComputationTimestamp(String computationTimestamp) {
        this.computationTimestamp = computationTimestamp;
    }

    public void setValueList(List<championshipValue> valueList) {
        this.valueList = valueList;
    }

    public void addValueToList(championshipValue f) {
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

