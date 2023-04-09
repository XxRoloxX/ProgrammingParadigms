import java.util.ArrayList;

public abstract class ObjectAnimation implements Video{

    protected Item itemToAnimate;
    protected ArrayList<Item> frames;

    protected int animationLength;

    public ObjectAnimation(Item itemToAnimate, int animationLength){
        this.itemToAnimate=itemToAnimate;
        frames = new ArrayList<>();
        this.animationLength=animationLength;
    }

   abstract protected void animationStep();

    public Item getNewFrame(){
        animationStep();
        return itemToAnimate;
    }


}
