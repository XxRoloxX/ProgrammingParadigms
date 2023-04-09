public class Tetrahedron extends Solid{
    private Point3D[] points;

    //private Point3D[] rotatedPoints;


    private Point3D originalCentroid;

    private Point3D[] vertices;



    public Tetrahedron(boolean filled, Point3D[] vertices){
        super(filled, new Point());
        this.vertices = vertices;
        originalCentroid = getCentroid();

        if(filled) {
            createFilledTetrahedron();
        }else{
            createEmptyTetrahedron();
        }


    }
    public Tetrahedron(boolean filled, Point3D vertices0, Point3D vertices1,Point3D vertices2,Point3D vertices3){
        super(filled,new Point());
        setPosition(position);
        this.vertices = new Point3D[]{vertices0, vertices1, vertices2, vertices3};
        originalCentroid = getCentroid();

        if(filled) {
            createFilledTetrahedron();
        }else{
            createEmptyTetrahedron();
        }



    }

    public void translateVertices(Point vector){
        for(int i=0;i<vertices.length;i++){
            vertices[i].translate(vector);
        }
    }

    private Point3D getCentroid(){

        Point3D centroid = new Point3D(0,0,0);
        for(int i=0;i<vertices.length;i++){
            centroid.add(vertices[i]);
        }
        centroid.multiply(1.0/4.0);

        return centroid;
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



    public void translateAllPoints(Point3D p){
        for(int i=0;i<points.length;i++){
            if(points[i]!=null)
                points[i].translate(p);
        }
    }
    public void translate(Point p){
        originalCentroid.translate(p);
    }



    private void createFilledTetrahedron(){

        double totalVolume = getVolume();


        points=new Point3D[(int)Math.ceil(totalVolume)];
        Point[] boundingBox = GeometryUtils.getBoundingBoxAroundPoints(vertices);
        int[] dephBorder = GeometryUtils.getDephBoundingBox(vertices);



        //translateVertices(GeometryUtils.multiply(originalCentroid,-1));

        int iter=0;

        for(int i=boundingBox[0].getX();i<=boundingBox[1].getX();i++){
            for(int j=boundingBox[0].getY();j<=boundingBox[2].getY();j++){
                for(int k=dephBorder[0];k<=dephBorder[1];k++){
                    Point3D newPoint = new Point3D(i,j,k);
                    if(isPointOnSurface(newPoint)){
                        points[iter]=newPoint;
                        iter++;
                    }

                }
            }
        }
        translateAllPoints(GeometryUtils.subtract3D(originalCentroid,new Point3D(0,0,0)));
    }
    private void createEmptyTetrahedron(){

        double totalVolume = getVolume();


        points=new Point3D[(int)Math.ceil(totalVolume)];
        Point[] boundingBox = GeometryUtils.getBoundingBoxAroundPoints(vertices);
        int[] depthBorder = GeometryUtils.getDephBoundingBox(vertices);


        int iter=0;

        for(int i=boundingBox[0].getX();i<=boundingBox[1].getX();i++){
            for(int j=boundingBox[0].getY();j<=boundingBox[2].getY();j++){
                for(int k=depthBorder[0];k<=depthBorder[1];k++){
                    Point3D newPoint = new Point3D(i,j,k);
                    if(isPointOnEdge(newPoint)){
                        points[iter]=newPoint;
                        iter++;
                    }

                }
            }
        }
        translateAllPoints(GeometryUtils.subtract3D(originalCentroid,new Point3D(0,0,0)));
    }


    private boolean isPointOnEdge(Point3D point){

        Point3D[] tetrahedron1 = {vertices[0],vertices[1],vertices[3],point};
        Point3D[] tetrahedron2 = {vertices[0],vertices[2],vertices[3],point};
        Point3D[] tetrahedron3 = {vertices[1],vertices[2],vertices[3],point};
        Point3D[] tetrahedron4 ={vertices[0],vertices[1],vertices[2],point};


        double Vol1 = GeometryUtils.getTetrahedronVolume(tetrahedron1);
        double Vol2 = GeometryUtils.getTetrahedronVolume(tetrahedron2);
        double Vol3 = GeometryUtils.getTetrahedronVolume(tetrahedron3);
        double Vol4 = GeometryUtils.getTetrahedronVolume(tetrahedron4);
        precision=0.02;

        double volumePrecision = getVolume()*precision;

        double volume = getVolume();

        if(Vol1+Vol2+Vol3+Vol4>getVolume()+volumePrecision){
            return false;
        }else if(Vol1<volumePrecision && Vol2<volumePrecision){
            point.setCharToDraw('#');
            return true;
        }else if(Vol2<volumePrecision && Vol3<volumePrecision){
            point.setCharToDraw('#');
            return true;
        }else if(Vol3<volumePrecision && Vol1<volumePrecision){
            point.setCharToDraw('#');
            return true;
        }else if(Vol4<volumePrecision && Vol1<volumePrecision){
            point.setCharToDraw('#');
            return true;
        }else if(Vol4<volumePrecision && Vol2<volumePrecision){
            point.setCharToDraw('#');
            return true;
        }else if(Vol4<volumePrecision && Vol3<volumePrecision) {
            point.setCharToDraw('#');
            return true;
        }

        return false;
    }
    private boolean isPointOnSurface(Point3D point){

        Point3D[] tetrahedron1 = {vertices[0],vertices[1],vertices[3],point};
        Point3D[] tetrahedron2 = {vertices[0],vertices[2],vertices[3],point};
        Point3D[] tetrahedron3 = {vertices[1],vertices[2],vertices[3],point};
        Point3D[] tetrahedron4 ={vertices[0],vertices[1],vertices[2],point};


        double Vol1 = GeometryUtils.getTetrahedronVolume(tetrahedron1);
        double Vol2 = GeometryUtils.getTetrahedronVolume(tetrahedron2);
        double Vol3 = GeometryUtils.getTetrahedronVolume(tetrahedron3);
        double Vol4 = GeometryUtils.getTetrahedronVolume(tetrahedron4);
        precision=0.05;

        double volumePrecision = getVolume()*precision;

        double volume = getVolume();

        if(Vol1+Vol2+Vol3+Vol4>getVolume()+volumePrecision){
            return false;
        }else if(Vol1<volumePrecision){
            point.setCharToDraw('#');
            return true;
        }else if(Vol2<volumePrecision){
            point.setCharToDraw('@');
            return true;
        }else if(Vol3<volumePrecision){
            point.setCharToDraw('i');
            return true;
        }else if(Vol4<volumePrecision){
            point.setCharToDraw('*');
            return true;
        }

        return false;

    }


    @Override
    public Point[] getBoundingBox() {
        translateAllPoints(GeometryUtils.subtract3D(new Point3D(0,0,0),originalCentroid));
        Point[] result = GeometryUtils.getBoundingBoxAroundPoints(points);
        translateAllPoints(GeometryUtils.subtract3D(originalCentroid,new Point3D(0,0,0)));
        return result;
    }


    @Override
    public void draw() {

        translateAllPoints(GeometryUtils.subtract3D(new Point3D(0,0,0),originalCentroid));
        for(int i=0;i<points.length;i++){
            if(points[i]!=null && isInsideScene(points[i].getX(), points[i].getY())){
                scene.getSceneField()[points[i].getY()][points[i].getX()]=points[i].charToDraw;
            }
        }
        translateAllPoints(GeometryUtils.subtract3D(originalCentroid,new Point3D(0,0,0)));

    }

    public double getVolume(){
        return Math.abs(GeometryUtils.dotProduct3D(
                GeometryUtils.crossProduct3D(GeometryUtils.subtract3D(vertices[1],vertices[2]),GeometryUtils.subtract3D(vertices[3],vertices[2])),
                GeometryUtils.subtract3D(vertices[0],vertices[2]))/6);
    }


}
