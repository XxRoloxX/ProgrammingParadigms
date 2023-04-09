public abstract class Solid extends Shape implements SolidInterface{


    public Solid(boolean filled, Point position){
        super(filled,position);
    }
    /*
    @Override
    public abstract void translate(Point p);

    @Override
    public abstract Point[] getBoundingBox();
*/

    @Override
    public abstract void draw();

    public abstract void rotatePointsOnXAxis(double angle);
    public abstract void rotatePointsOnYAxis(double angle);
    public abstract void rotatePointsOnZAxis(double angle);
}
