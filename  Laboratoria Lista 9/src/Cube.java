import java.util.Arrays;

public class Cube extends Solid{

    private int width;
    private double rotationX;
    private double rotationY;

    private double rotationZ;
    private Point3D[] points;

    //private Point3D[] rotatedPoints;

    private Point normalizedPosition;



    public Cube(boolean filled, Point position, int width){
        super(filled,position);
        this.width=width;
        this.rotationX = 0;
        this.rotationY = 0;
        this.rotationZ = 0;
        setPosition(position);
        normalizedPosition = new Point(-width/2, -width/2);


        if(filled) {
            createFilledCube();
        }else{
            createEmptyCube();
        }


        //rotatePointsOnZAxis(rotation);
        //rotatePointsOnYAxis(rotation);
        //rotatePointsOnXAxis(rotation);

    }

    public void rotatePointsOnXAxis(double angle){
        /*
        rotationX+=angle;
        if(rotationX>=2*Math.PI){
            rotationX=rotationX-2*Math.PI;
        }
        updateRotation();
        */

        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
            points[i].rotateX(angle);
        }


    }
    public void rotatePointsOnYAxis(double angle){
        /*
        rotationY+=angle;
        if(rotationY>=2*Math.PI){
            rotationY=rotationY-2*Math.PI;
        }
        updateRotation();
        */

        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
            points[i].rotateY(angle);
        }
    }
    public void rotatePointsOnZAxis(double angle){
        /*
        rotationZ+=angle;
        if(rotationZ>=2*Math.PI){
            rotationZ=rotationZ-2*Math.PI;
        }
        updateRotation();
        */

        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
            points[i].rotateZ(angle);
        }
    }


/*
    public void updateRotation(){
        double halfPI = Math.PI/2;
        System.out.println(halfPI);

        double tmpAngle;
        for(int i=0;i<points.length;i++){

            if(points[i]!=null){
                rotatedPoints[i] = new Point3D(points[i]);

                tmpAngle=halfPI;
                while(tmpAngle<rotationX){
                    //System.out.println("rotationX: "+rotationX+"OoioiioioioX: "+tmpAngle);
                    rotatedPoints[i].rotateX90deg();
                    tmpAngle=tmpAngle+(halfPI);
                }

                rotatedPoints[i].rotateX(rotationX-(tmpAngle-halfPI));

                tmpAngle=halfPI;
                while(tmpAngle<rotationY){
                    rotatedPoints[i].rotateY90deg();
                    tmpAngle=tmpAngle+(halfPI);
                   // System.out.println("rotationY: "+rotationY+"OoioiioioioX: "+tmpAngle);
                }

                rotatedPoints[i].rotateY(rotationY-(tmpAngle-halfPI));

                tmpAngle=halfPI;
                while(tmpAngle<rotationZ){
                    //rotatedPoints[i].rotateZ90deg();
                    tmpAngle=tmpAngle+(halfPI);
                    //System.out.println("rotationZ: "+rotationZ+"OoioiioioioZ: "+tmpAngle);

                }

                rotatedPoints[i].rotateZ(rotationZ-(tmpAngle-halfPI));


            }

        }
    }

 */
    public void translateAllPoints(Point p){
        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
                points[i].translate(p);
        }
    }
    public void translate(Point p){
        position.translate(p);
    }

    private void createFilledCube(){

        points=new Point3D[width*width*6];

        int iter=0;

        for(int i=normalizedPosition.getX();i<-normalizedPosition.getX();i++){
            for(int j=normalizedPosition.getY();j<-normalizedPosition.getY();j++){
                for(int k=-width/2;k<width/2;k++){
                    if(isPointOnSurface(new Point3D(j,i,k))){
                        points[iter]=new Point3D(j,i,k);
                        if((points[iter].getX()==normalizedPosition.getX() || points[iter].getX()==-normalizedPosition.getX()-1)){
                            points[iter].setCharToDraw('?');
                        }else if(points[iter].getY()==normalizedPosition.getY()|| points[iter].getY()==-normalizedPosition.getX()-1){
                            points[iter].setCharToDraw('*');

                        }else{
                            points[iter].setCharToDraw('@');
                        }



                        iter++;
                    }

                }
            }
        }
    }
    private void createEmptyCube(){

        int iter=0;
        points = new Point3D[12*width];

        for(int i=normalizedPosition.getX();i<-normalizedPosition.getX();i++){
            for(int j=normalizedPosition.getY();j<-normalizedPosition.getX();j++){
                for(int k=-width/2;k<width/2;k++){

                    if(isPointOnEdge(new Point3D(j,i,k))){
                        points[iter]=new Point3D(j,i,k);
                        points[iter].setCharToDraw('#');
                        iter++;
                        //System.out.println(points[iter-1]);
                    }
                }
            }
        }
        //System.out.println(iter);
    }


    private boolean isPointOnEdge(Point3D point){

        if((point.getX()==normalizedPosition.getX() || point.getX()==-normalizedPosition.getX()-1) && (point.getY()==normalizedPosition.getY() || point.getY()==-normalizedPosition.getX()-1)){
            return true;
        }else if((point.getX()==normalizedPosition.getX() || point.getX()==-normalizedPosition.getX()-1) && (point.getZ()==-width/2 || point.getZ()==width/2 -1)){
            return true;
        }else if((point.getY()==normalizedPosition.getY() || point.getY()==-normalizedPosition.getY()-1) && (point.getZ()==-width/2 || point.getZ()==width/2 -1)){
            return true;
        }else{
            return false;
        }

    }
    private boolean isPointOnSurface(Point3D point){
        if((point.getX()==normalizedPosition.getX() || point.getX()==-normalizedPosition.getX()-1) || (point.getY()==normalizedPosition.getY() || point.getY()==-normalizedPosition.getX()-1) ||(point.getZ()==-width/2 || point.getZ()==width/2 -1)){
            return true;
        }else{
            return false;
        }
    }


    @Override
    public Point[] getBoundingBox() {
        translateAllPoints(GeometryUtils.subtract(normalizedPosition,position));
        Point[] result = GeometryUtils.getBoundingBoxAroundPoints(points);
        translateAllPoints(GeometryUtils.subtract(position,normalizedPosition));
        return result;
    }


    @Override
    public void draw() {

        translateAllPoints(GeometryUtils.subtract(normalizedPosition,position));
        for(int i=0;i<points.length;i++){
            if(points[i]!=null && isInsideScene(points[i].getX(), points[i].getY())){
                scene.getSceneField()[points[i].getY()][points[i].getX()]=points[i].charToDraw;
            }
        }
        translateAllPoints(GeometryUtils.subtract(position,normalizedPosition));

    }

}
