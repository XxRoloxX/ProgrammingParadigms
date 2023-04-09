import jdk.jfr.Description;

import java.util.ArrayList;

public class Triangle extends Shape {

    private Point p1;
    private Point p2;
    private Point p3;

    public Triangle(boolean filled,Point p1, Point p2, Point p3){
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.setFilled(filled);

        Point[] points = new Point[3];
        points[0] = p1;
        points[1] = p2;
        points[2] = p3;
        points = GeometryUtils.getBoundingBoxAroundPoints(points);

        this.position = points[0];

    }
    public void translate(Point o){
        position.translate(o);
        p1.translate(o);
        p2.translate(o);
        p3.translate(o);
    }

    @Override
    public Point[] getBoundingBox() {
        Point[] points = new Point[3];
        points[0] = new Point(p1);
        points[1] = new Point(p2);
        points[2] = new Point(p3);
        return GeometryUtils.getBoundingBoxAroundPoints(points);
    }
    @Description("Returns 1 if point is in triangle, 0 if point is on edge, and -1 if point is outside the triangle")
    public int isPointInsideTriangle(Point pOther){

        Triangle triangle1 = new Triangle(false,p1,p2,pOther);
        Triangle triangle2 = new Triangle(false,p1,p3,pOther);
        Triangle triangle3 = new Triangle(false,p2,p3,pOther);

        double areaSum = triangle1.triangleArea() + triangle2.triangleArea() + triangle3.triangleArea();
        double totalArea = triangleArea();
        double areaPrecision = precision*totalArea;

        if(Math.abs(totalArea-areaSum)>areaPrecision){
            return -1;
        }else if(triangle1.triangleArea()<areaPrecision || triangle2.triangleArea()<areaPrecision || triangle3.triangleArea()<areaPrecision){
            return 0;
        }else{
            return 1;
        }

    }
    public double triangleArea(){
        Point vecP2P1 = GeometryUtils.subtract(p2,p1);
        Point vecP3P1 = GeometryUtils.subtract(p3,p1);
        return Math.abs(GeometryUtils.crossProduct(vecP2P1,vecP3P1));
    }

    @Override
    public void draw() {

        Point[] boundingBox = getBoundingBox();


        for(int i=boundingBox[0].getY();i<boundingBox[2].getY();i++){
            for(int j=boundingBox[0].getX();j<boundingBox[3].getX();j++){

                if(isInsideScene(j,i) && (isPointInsideTriangle(new Point(j,i))>=0 && filled || isPointInsideTriangle(new Point(j,i))==0)){
                    scene.getSceneField()[i][j]=charToDraw;
                }
            }
        }


        /*
        for(int i=0;i<sceneHeight;i++){
            for(int j=0;j<sceneWidth;j++){

                if(isInsideScene(j,i) && (isPointInsideTriangle(new Point(j,i))>=0 && filled || isPointInsideTriangle(new Point(j,i))==0)){
                    sceneField[i][j]=charToDraw;
                }
            }
        }
        */

    }

    public Point getP1() {
        return p1;
    }

    public void setP1(Point p1) {
        this.p1 = p1;
    }

    public Point getP2() {
        return p2;
    }

    public void setP2(Point p2) {
        this.p2 = p2;
    }

    public Point getP3() {
        return p3;
    }

    public void setP3(Point p3) {
        this.p3 = p3;
    }
}
