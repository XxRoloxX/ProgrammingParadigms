public class SolidDecorator extends ItemDecorator implements SolidInterface{

    public SolidDecorator(ItemInterface decoratedItem){
        super(decoratedItem);
    }

    public void rotatePointsOnXAxis(double angle){
        //if(decoratedItem instanceof SolidInterface)
            ((SolidInterface)decoratedItem).rotatePointsOnXAxis(angle);
    }
    public  void rotatePointsOnYAxis(double angle){
       // if(decoratedItem instanceof SolidInterface)
            ((SolidInterface)decoratedItem).rotatePointsOnYAxis(angle);
    }
    public  void rotatePointsOnZAxis(double angle){
       // if(decoratedItem instanceof SolidInterface)
            ((SolidInterface)decoratedItem).rotatePointsOnZAxis(angle);
    }
}
