public class Tetrahedron extends Shape{
    private int length;
    private Point3D[] points;

    //private Point3D[] rotatedPoints;

    private Point normalizedPosition;

    private Point3D[] vertices;



    public Tetrahedron(boolean filled, Point position, int length){
        super(filled,position);
        this.length=length;
        setPosition(position);
        normalizedPosition = new Point(-length/2, -length/2);
        vertices = new Point3D[4];


        if(filled) {
            createFilledTetrahedron();
        }else{
            createEmptyTetrahedron();
        }


    }

    public void rotatePointsOnXAxis(double angle){


        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
                points[i].rotateX(angle);
        }


    }
    public void rotatePointsOnYAxis(double angle){


        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
                points[i].rotateY(angle);
        }
    }
    public void rotatePointsOnZAxis(double angle){


        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
                points[i].rotateZ(angle);
        }
    }



    public void translate(Point p){
        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
                points[i].translate(p);
        }
    }

    private void createFilledTetrahedron(){

        points=new Point3D[length*length*length];

        int iter=0;

        for(int i=normalizedPosition.getX();i<-normalizedPosition.getX();i++){
            for(int j=normalizedPosition.getY();j<-normalizedPosition.getY();j++){
                for(int k=-length/2;k<length/2;k++){
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
    private void createEmptyTetrahedron(){

        int iter=0;
        points = new Point3D[12*length];

        for(int i=normalizedPosition.getX();i<-normalizedPosition.getX();i++){
            for(int j=normalizedPosition.getY();j<-normalizedPosition.getX();j++){
                for(int k=-length/2;k<length/2;k++){

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
        }else if((point.getX()==normalizedPosition.getX() || point.getX()==-normalizedPosition.getX()-1) && (point.getZ()==-length/2 || point.getZ()==length/2 -1)){
            return true;
        }else if((point.getY()==normalizedPosition.getY() || point.getY()==-normalizedPosition.getY()-1) && (point.getZ()==-length/2 || point.getZ()==length/2 -1)){
            return true;
        }else{
            return false;
        }

    }
    private boolean isPointOnSurface(Point3D point){
        return false;
    }


    @Override
    public Point[] getBoundingBox() {
        translate(GeometryUtils.subtract(normalizedPosition,position));
        Point[] result = GeometryUtils.getBoundingBoxAroundPoints(points);
        translate(GeometryUtils.subtract(position,normalizedPosition));
        return result;
    }


    @Override
    public void draw() {

        translate(GeometryUtils.subtract(normalizedPosition,position));
        for(int i=0;i<points.length;i++){
            if(points[i]!=null && isInsideScene(points[i].getX(), points[i].getY())){
                sceneField[points[i].getY()][points[i].getX()]=points[i].charToDraw;
            }
        }
        translate(GeometryUtils.subtract(position,normalizedPosition));

    }


}
