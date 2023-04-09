public class SolidRotation extends ObjectAnimation{

    private double xAxisRotation;
    private double yAxisRotation;
    private double zAxisRotation;

    public SolidRotation(SolidInterface cube, int length){
        super(cube, length);
         xAxisRotation = 0;
         yAxisRotation=0;
         zAxisRotation=0;
    }
    public void setRotationAngles(double x, double y, double z){
        xAxisRotation=x;
        yAxisRotation=y;
        zAxisRotation=z;
    }

    protected void animationStep() {
        ((SolidInterface)itemToAnimate).rotatePointsOnYAxis(xAxisRotation);
        ((SolidInterface)itemToAnimate).rotatePointsOnXAxis(yAxisRotation);
        ((SolidInterface)itemToAnimate).rotatePointsOnZAxis(zAxisRotation);
    }



}
