package it.unipi.dii.analyticsPeriodicCalculator.analyticsResultModel.financialResults;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Objects;

public class financialValue {
    private String dateTo;
    private String dateFrom;
    private double value;

    public financialValue(String dateTo, String dateFrom, double value) {
        this.dateTo = dateTo;
        this.dateFrom = dateFrom;
        this.value = value;
    }

    public financialValue() {
    }


    @JsonProperty("dateTo")
    public String getDateTo() {
        return this.dateTo;
    }

    @JsonProperty("dateFrom")
    public String getDateFrom() {
        return this.dateFrom;
    }
    @JsonProperty("value")
    public double getValue() {
        return this.value;
    }


    public void setDateTo(String dateTo) {
        this.dateTo = dateTo;
    }

    public void setDateFrom(String dateFrom) {
        this.dateFrom = dateFrom;
    }

    public void setValue(double value) {
        this.value = value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(dateTo, dateFrom, value);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        financialValue that = (financialValue) obj;
        return Double.compare(that.value, value) == 0 &&
                Objects.equals(dateTo, that.dateTo) &&
                Objects.equals(dateFrom, that.dateFrom);
    }

    @Override
    public String toString() {
        return "{" +
                "dateTo:'" + this.dateTo + '\'' +
                ", dateFrom:'" + this.dateFrom + '\'' +
                ", value:" + this.value +
                '}';
    }
}
