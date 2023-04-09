public class GeometryUtils {

    final static double DEFAULT_PRECISION=0.5;
    final static char DEFAULT_CHAR = '#';

    final static int DEFAULT_TRESHHOLD = -16000000;
    public static double distanceBetweenPoints(Point p1, Point p2){
       return  p1.getDistance(p2);
    }

    public static double distanceBetweenCoordinates(int x1, int y1, int x2, int y2){
        Point p1 = new Point(x1,y1);
        Point p2 = new Point(x2,y2);
        return p1.getDistance(p2);
    }
    public static Point[] getBoundingBoxAroundPoints(Point[] points){
        int left = points[0].getX();
        int right = points[0].getX();
        int top = points[0].getY();
        int bottom = points[0].getY();

        for(int i=1;i<points.length;i++){
            if(points[i]!=null){
                if(points[i].getX()<left){
                    left = points[i].getX();
                }
                if(points[i].getX()>right){
                    right = points[i].getX();
                }
                if(points[i].getY()<top){
                    top = points[i].getY();
                }
                if(points[i].getY()>bottom){
                    bottom = points[i].getY();
                }
            }
        }

        Point[] boundingBox = new Point[4];
        boundingBox[0] = new Point(left,top);
        boundingBox[1] = new Point(right,top);
        boundingBox[2] = new Point(left, bottom);
        boundingBox[3] = new Point(right, bottom);

        return boundingBox;
    }

    // [0] - smallest value in z-axis, [1] - biggest value in z-axis
    public static int[] getDephBoundingBox(Point3D[] points){
        int min = points[0].getZ();
        int max = points[0].getZ();

        for(int i=1;i<points.length;i++){

            if(points[i].getZ()<min){
                min=points[i].getZ();
            }

            else if(points[i].getZ()>max){
                max=points[i].getZ();
            }

        }
        int[] result = new int[2];
        result[0] = min;
        result[1] = max;

        return result;

    }

    public static Point add(Point p1, Point p2){
        Point result = new Point(p1.getX()+ p2.getX(),p1.getY()+ p2.getY());
        return result;
    }
    public static Point multiply(Point p1, double multiplicator){
        Point result = new Point(p1);
        result.multiply(multiplicator);
        //result.multiply(multiplicator);
        return result;
    }
    public static Point subtract(Point p1, Point p2){
        Point result = new Point(p2.getX()-p1.getX(), p2.getY() - p1.getY());
        return result;
    }
    public static Point3D subtract3D(Point3D p1, Point3D p2){
        Point3D result = new Point3D(p2.getX()-p1.getX(), p2.getY() - p1.getY(),p2.getZ()-p1.getZ());
        return result;
    }

    public static double crossProduct(Point p1, Point p2){
        return p1.getX()*p2.getY() - p1.getY()*p2.getX();
    }


    public static Point3D crossProduct3D(Point3D p1, Point3D p2){

        return new Point3D((int)(p1.y*p2.z - p1.z*p2.y),(int)(-p1.x*p2.z + p1.z*p2.x),(int)(p1.x*p2.y-p1.y*p2.x));
    }

    public static double dotProduct3D(Point3D p1, Point3D p2){

        return p1.x*p2.x + p1.y*p2.y + p1.z*p2.z;


    }

    public static int binomialCoefficient(int n, int i){
        return factorial(n) / (factorial(i)*factorial(n-i));
    }
    public static  int factorial(int n){

        int result=1;

        for(int i=2;i<=n;i++){
            result*=i;
        }

        return result;

    }

    public static double getTetrahedronVolume(Point3D[] vertices){

        return Math.abs(GeometryUtils.dotProduct3D(
                GeometryUtils.crossProduct3D(GeometryUtils.subtract3D(vertices[1],vertices[2]),GeometryUtils.subtract3D(vertices[3],vertices[2])),
                GeometryUtils.subtract3D(vertices[0],vertices[2]))/6);





    }

}
