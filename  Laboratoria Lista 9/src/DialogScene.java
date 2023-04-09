import org.w3c.dom.Text;

import java.util.ArrayList;

public class DialogScene extends Scene{


    public DialogScene(int width, int height){
        super(width,height);
    }

    public boolean addOption(TextItem option, Item representedItem){
        if(option!=null){
            DialogOption newOption = new DialogOption(option,representedItem);
            if(items.size()>0){
               Point[] boundingBox = items.get(items.size()-1).getBoundingBox();
               newOption.setPosition(boundingBox[2]);
            }
           // newOption.setText("   [ ]"+newOption.getText());
            addItem(newOption);
            return true;
        }else{
            return false;
        }
    }
    public boolean highlightOption(int index, boolean selected){
        if(index>=0 && index<items.size()){
            ((DialogOption)items.get(index)).setHighlighted(selected);
            return true;
        }else{
            return false;
        }
    }
    public void unhighlightAll(){
        for(ItemInterface option: items){
            ((DialogOption)option).setHighlighted(false);
        }
    }
    public void selectOption(DialogOption option){
        String oldString = option.getText();
        String[] splitedString = oldString.split("]");
        option.setText("   [*]"+splitedString[1]);
    }


}
