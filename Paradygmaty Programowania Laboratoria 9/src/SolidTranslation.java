public class SolidTranslation extends ObjectAnimation{

    Point movementVector;
    public SolidTranslation(Point initialVector, SolidInterface solidToAnimate, int animationLength){
        super(solidToAnimate,animationLength);
        movementVector = initialVector;
    }
    public SolidTranslation(SolidInterface solidToAnimate, int animationLength){
        super(solidToAnimate,animationLength);
        movementVector = new Point(1,1);
    }
    @Override
    protected void animationStep() {
        Point[] boundingBox = itemToAnimate.getBoundingBox();

        //Solid is above top border of scene
        if(itemToAnimate.getScene()==null){
            return;
        }
        if(boundingBox[0].getY()<0){

            movementVector.setY(Math.abs(movementVector.getY()));

        }else if(boundingBox[2].getY()>itemToAnimate.getScene().getHeight()){
            movementVector.setY(-Math.abs(movementVector.getY()));
        }

        if(boundingBox[0].getX()<0){

            movementVector.setX(Math.abs(movementVector.getX()));

        }else if(boundingBox[1].getX()>itemToAnimate.getScene().getWidth()){
            movementVector.setX(-Math.abs(movementVector.getX()));
        }




        Point oldPosition = new Point(itemToAnimate.getPosition());

        itemToAnimate.translate(movementVector);
        itemToAnimate.setPosition(GeometryUtils.add(oldPosition,movementVector));


    }
}
