import java.util.ArrayList;

public abstract class ObjectAnimation implements Video{

    protected ItemInterface itemToAnimate;
    protected ArrayList<ItemInterface> frames;

    protected int animationLength;

    public ObjectAnimation(ItemInterface itemToAnimate, int animationLength){
        this.itemToAnimate=itemToAnimate;
        frames = new ArrayList<>();
        this.animationLength=animationLength;
    }

   abstract protected void animationStep();

    public ItemInterface getNewFrame(){
        animationStep();
        return itemToAnimate;
    }

    public ItemInterface getItemToAnimate() {
        return itemToAnimate;
    }
}
