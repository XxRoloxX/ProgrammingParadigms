public class BoundingBoxDecorator extends ItemDecorator{
    public BoundingBoxDecorator(ItemInterface decoratedItem){
        super(decoratedItem);
    }
    @Override
    public void draw(){
        decoratedItem.draw();

        Point[] boundingBox = decoratedItem.getBoundingBox();
        Item rect = new Rect(boundingBox[0],false,boundingBox[1].getX()-boundingBox[0].getX(),boundingBox[2].getY()-boundingBox[0].getY());
        rect.scene = decoratedItem.getScene();
        rect.draw();
    }
}
