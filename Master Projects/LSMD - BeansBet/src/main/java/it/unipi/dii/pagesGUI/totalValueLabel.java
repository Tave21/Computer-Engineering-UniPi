package it.unipi.dii.pagesGUI;

import javafx.scene.control.Label;

public class totalValueLabel {
    private final int slipID;
    private final Label totalLabel;
    private double multiplierProduct;

    public totalValueLabel(int slipID, Label totalLabel) {
        this.slipID = slipID;
        this.totalLabel = totalLabel;
    }
    public Label getTotalLabel() {
        return totalLabel;
    }
    public int getSlipID() {
        return slipID;
    }
    public double getMultiplierProduct() {
        return multiplierProduct;
    }
    public void setMultiplierProduct(double betAmount) {
        this.multiplierProduct = betAmount;
    }

}