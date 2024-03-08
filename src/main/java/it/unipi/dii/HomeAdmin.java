package it.unipi.dii;

import it.unipi.dii.pagesGUI.navBar;
import javafx.scene.layout.HBox;

public class HomeAdmin {
    private BeansBetGUI beansBetGUI;
    public HomeAdmin(BeansBetGUI beansBetGUI) {
        this.beansBetGUI = beansBetGUI;
    }
    public HBox getHeader() {
        navBar buttonHeader = new navBar(beansBetGUI);
        HBox header = new HBox();
        return buttonHeader.createHeader(header, 3);
    }
}