import java.util.ArrayList;

public class DialogOption extends TextItem{

    private boolean chosen;

    private boolean highlighted;

    private Item representedItem;
    public DialogOption(Point position, String text, int fontSize,int width, int height,Item representedItem){
        super(position,text,fontSize,width,height);
        this.representedItem=representedItem;
        chosen=false;
        highlighted=false;
        updateCheckmark();

    }
    public DialogOption(TextItem item, Item representedItem){
        super(item.position,item.text,item.fontSize,item.width,item.height);
        this.representedItem = representedItem;
        chosen=false;
        highlighted=false;
        updateCheckmark();

    }

    public boolean isChosen() {
        return chosen;
    }

    public void setChosen(boolean chosen) {
        this.chosen = chosen;
        updateCheckmark();

    }

    public boolean isHighlighted() {
        return highlighted;
    }

    public void setHighlighted(boolean highlighted) {
        this.highlighted = highlighted;
    }

    public Item getRepresentedItem() {
        return representedItem;
    }

    public void setRepresentedItem(Item representedItem) {
        this.representedItem = representedItem;
    }

    @Override
    public void draw(){
        drawAdvanced(treshhold,highlighted);
    }

    public void updateCheckmark(){
        String oldString = text;
        String[] splitedString = oldString.split(": ");
        System.out.println(splitedString[0]);
        if(splitedString.length>1){
            setText("   ["+(chosen?"*":" ")+ "]: "+splitedString[1]);
        }else{
            setText("   ["+(chosen?"*":" ")+ "]: "+splitedString[0]);
        }

    }
}
