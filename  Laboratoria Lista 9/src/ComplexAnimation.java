import java.util.ArrayList;

public class ComplexAnimation extends ObjectAnimation{

    ArrayList<ObjectAnimation> animations;

    public ComplexAnimation(ItemInterface itemToAnimate, int animationLength){
        super(itemToAnimate,animationLength);
        animations = new ArrayList<>();

    }

    public void addAnimation(ObjectAnimation newAnimation){
        animations.add(newAnimation);
    }

    @Override
    protected void animationStep() {
        for(ObjectAnimation animation: animations){
            animation.animationStep();
        }
    }
}
