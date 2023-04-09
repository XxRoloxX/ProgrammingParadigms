import java.util.ArrayList;
import java.util.Iterator;

public class CubeAnimation extends ObjectAnimation{

    private double xAxisRotation;
    private double yAxisRotation;
    private double zAxisRotation;

    public CubeAnimation(Item cube, int length){
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
        ((Cube)itemToAnimate).rotatePointsOnYAxis(xAxisRotation);
        ((Cube)itemToAnimate).rotatePointsOnXAxis(yAxisRotation);
        ((Cube)itemToAnimate).rotatePointsOnZAxis(zAxisRotation);
    }



}
