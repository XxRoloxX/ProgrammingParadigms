public class Segment extends Primitive{
    Point start;
    Point end;
    public Segment(Point start, Point end){

        Point topLeftCorner = new Point(0,0);

        if(topLeftCorner.getDistance(start)<topLeftCorner.getDistance(end)){
            setPosition(start);
        }else{
            setPosition(end);
        }

        this.start = start;
        this.end = end;
    }
    public Segment(){
        start = new Point();
        end = new Point();
    }

    public Point getStart() {
        return start;
    }

    public void setStart(Point start) {
        this.start = start;
    }

    public Point getEnd() {
        return end;
    }

    public void setEnd(Point end) {
        this.end = end;
    }

    public double getLength(){
        return start.getDistance(end);
    }

    @Override
    public Point[] getBoundingBox() {
        /*
        int left;
        int right;
        int top;
        int bottom;

        if(start.getX() < end.getX()){
            left = start.getX();
            right = end.getX();
        }else{
            right = start.getX();
            left = end.getX();
        }

        if(start.getY() < end.getY()){
            top = start.getX();
            bottom = end.getX();
        }else{
            bottom = start.getX();
            top = end.getX();
        }

        Point[] boundingBox = new Point[4];

        boundingBox[0] = new Point(left,top);
        boundingBox[1] = new Point(right,top);
        boundingBox[2] = new Point(left,bottom);
        boundingBox[3] = new Point(right,bottom);

        return boundingBox;
         */
        Point[] points = new Point[2];
        points[0] = start;
        points[1]= end;
        return GeometryUtils.getBoundingBoxAroundPoints(points);

    }
    public void translate(Point o){
        position.translate(o);
        start.translate(o);
        end.translate(o);
    }

    @Override
    public void draw() {
        double a;
        double b;
       // y = ax +b
       // b = y-ax


        Point[] boundingBox = getBoundingBox();

        if(end.getX()-start.getX()!=0) {
            a = (end.getY() - start.getY()) / (end.getX() - start.getX());
            b = end.getY() - a * end.getX();

            for (int i = boundingBox[0].getY(); i <= boundingBox[2].getY(); i++) {
                for (int j = boundingBox[0].getX(); j <= boundingBox[1].getX(); j++) {

                    if (Math.abs(a * j + b - i) < precision) {
                        if(isInsideScene(j,i)){
                            sceneField[i][j] = charToDraw;
                        }
                    }
                }
            }
        }else{
            for(int i=boundingBox[0].getY();i<boundingBox[2].getY();i++){
                if(isInsideScene(start.getX(),i)){
                    sceneField[i][start.getX()]=charToDraw;
                }

            }
        }

    }
}
