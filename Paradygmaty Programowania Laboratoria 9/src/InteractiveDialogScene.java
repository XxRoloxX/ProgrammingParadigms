import java.awt.*;
import java.util.ArrayList;
import java.util.Scanner;

public class InteractiveDialogScene {
    private DialogScene scene;

    public InteractiveDialogScene(int width, int height){
        scene = new DialogScene(width, height);
    }

    public void addOption(TextItem option, Item representedItem){
        scene.addOption(option,representedItem);
    }

    public void runScene(){
        Scanner scan = new Scanner(System.in);
        char keyPressed;
        boolean userFinishedInput=false;
        DialogOption currentlyHighlightedItem = (DialogOption) scene.getItem(0);

        while(!userFinishedInput){
            ImageToASCII.clearScreen();
            scene.clearScene();
            scene.draw();
            keyPressed = scan.next().charAt(0);
            switch(keyPressed){
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    scene.unhighlightAll();
                    if(scene.getItem(Integer.parseInt(String.valueOf(keyPressed))-1)!=null){
                        currentlyHighlightedItem=(DialogOption) scene.getItem(Integer.parseInt(String.valueOf(keyPressed))-1);
                        scene.unhighlightAll();
                        currentlyHighlightedItem.setHighlighted(true);
                    }

                    break;

                case 's':
                    if(currentlyHighlightedItem!=null){
                        currentlyHighlightedItem.setChosen(!currentlyHighlightedItem.isChosen());

                    }
                    break;
                case 'q':
                    userFinishedInput=true;
                    break;

            }


        }


    }
    public ArrayList<ItemInterface> getItems(){
        return scene.getItems();
    }
}
