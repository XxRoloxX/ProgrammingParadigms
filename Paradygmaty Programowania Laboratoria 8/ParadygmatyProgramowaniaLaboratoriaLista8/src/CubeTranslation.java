public class CubeTranslation extends ObjectAnimation{

    Point movementVector;
    public CubeTranslation(Point initialVector, Cube cubeToAnimate, int animationLength){
        super(cubeToAnimate,animationLength);
        movementVector = initialVector;
    }
    public CubeTranslation(Cube cubeToAnimate, int animationLength){
        super(cubeToAnimate,animationLength);
        movementVector = new Point(1,1);
    }
    @Override
    protected void animationStep() {
        Point[] boundingBox = itemToAnimate.getBoundingBox();

        for(Point point: boundingBox){
            System.out.println(point);
        }

        //Cube is above top border of scene

        if(boundingBox[0].getY()<0){

            movementVector.setY(Math.abs(movementVector.getY()));

        }else if(boundingBox[2].getY()>itemToAnimate.sceneHeight){
            movementVector.setY(-Math.abs(movementVector.getY()));
        }

        if(boundingBox[0].getX()<0){

            movementVector.setX(Math.abs(movementVector.getX()));

        }else if(boundingBox[1].getX()>itemToAnimate.sceneWidth){
            movementVector.setX(-Math.abs(movementVector.getX()));
        }




        Point oldPosition = new Point(itemToAnimate.position);

        itemToAnimate.translate(movementVector);
        itemToAnimate.setPosition(GeometryUtils.add(oldPosition,movementVector));


    }
}
