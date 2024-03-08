package it.unipi.dii.pagesGUI;

import javafx.scene.control.Label;

public class totalValueLabel {

    private int slipID;
    private Label totalLabel;
    private double multiplierProduct;

    public totalValueLabel(int slipID, Label totalLabel) {
        this.slipID = slipID;
        this.totalLabel = totalLabel;
    }
    public totalValueLabel(int slipID, Label totalLabel, double betAnmount) {
        this.slipID = slipID;
        this.totalLabel = totalLabel;
        this.multiplierProduct = multiplierProduct;
    }
    public Label getTotalLabel() {
        return totalLabel;
    }
    public void setTotalLabel(Label totalLabel) {
        this.totalLabel = totalLabel;
    }

    public int getSlipID() {
        return slipID;
    }

    public void setSlipID(int slipID) {
        this.slipID = slipID;
    }

    public double getMultiplierProduct() {
        return multiplierProduct;
    }
    public void setMultiplierProduct(double betAmount) {
        this.multiplierProduct = betAmount;
    }

}