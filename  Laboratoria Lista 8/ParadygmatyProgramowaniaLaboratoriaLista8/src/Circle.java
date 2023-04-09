public class Circle extends Shape {
    int radius;
    public Circle(){
        this.radius=0;
    }
    public Circle(boolean filled, Point position, int radius){
        super(filled, position);
        this.radius=radius;
    }

    @Override
    public Point[] getBoundingBox() {

        Point[] boundingBox = new Point[4];
        boundingBox[0] = new Point(position);
        boundingBox[1] = new Point(position.getX()+2*radius, position.getY());
        boundingBox[2] = new Point(position.getX(), position.getY() + 2*radius);
        boundingBox[3] = new Point(position.getX() + 2*radius, position.getY() + 2*radius);

        return boundingBox;
    }

    @Override
    public void draw() {
        Point[] boundingBox = getBoundingBox();
        Point middle = new Point((position.getX() +radius), (position.getY() + radius));
        /*
        System.out.print("Middle: ");
        System.out.println(middle);
        System.out.print("Position: ");
        System.out.println(position);

         */
        for(int i=0;i<boundingBox.length;i++){
            System.out.println(boundingBox[i]);
        }

        for(int i=boundingBox[0].getY();i<=boundingBox[2].getY();i++) {
            for (int j = boundingBox[0].getX(); j <= boundingBox[1].getX(); j++) {

                if (getFilled() == true && Math.abs(GeometryUtils.distanceBetweenCoordinates(j,i, middle.getX(), middle.getY()))<=radius
                        || Math.abs(GeometryUtils.distanceBetweenCoordinates(j,i, middle.getX(), middle.getY())-radius) < precision) {
                    if(isInsideScene(j,i)){
                        sceneField[i][j] = charToDraw;
                    }
                }
            }
        }
    }

    public int getRadius() {
        return radius;
    }

    public void setRadius(int radius) {
        this.radius = radius;
    }

    public void translate(Point o){
        position.translate(o);
    }
}
