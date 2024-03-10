package it.unipi.dii;

import it.unipi.dii.pagesGUI.navBar;
import javafx.scene.layout.HBox;

public class HomeRegistered {
    private final BeansBetGUI beansBetGUI;
    public HomeRegistered(BeansBetGUI beansBetGUI) {
        this.beansBetGUI = beansBetGUI;
    }
    public HBox getHeader() {
        navBar buttonHeader = new navBar(beansBetGUI);
        HBox header = new HBox();
        header = buttonHeader.createHeader(header, 2);

        return header;
    }
}