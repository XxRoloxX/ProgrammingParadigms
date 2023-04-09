public abstract class Item implements Comparable<Item> {

    protected Point position;
    protected char[][] sceneField;

    protected int deph;

    protected int sceneWidth;
    protected int sceneHeight;

    protected double precision;
    protected char charToDraw;

    public Item(){
        position = new Point();
        sceneField=null;
        precision = GeometryUtils.DEFAULT_PRECISION;
        charToDraw = GeometryUtils.DEFAULT_CHAR;
        deph=0;
    }
    public Item(Point p){

        position = new Point(p);
        sceneField = null;
        precision = GeometryUtils.DEFAULT_PRECISION;
        charToDraw = GeometryUtils.DEFAULT_CHAR;
        deph=0;
    }
    public Item(int x, int y){

        position = new Point(x,y);
        sceneField=null;
        precision = GeometryUtils.DEFAULT_PRECISION;
        charToDraw = GeometryUtils.DEFAULT_CHAR;
        deph=0;
    }

    public void setScene(char[][] sceneField, int sceneHeight, int sceneWidth){

        this.sceneField = sceneField;
        this.sceneHeight = sceneHeight;
        this.sceneWidth = sceneWidth;

    }

    @Override
    public int compareTo(Item o) {
        if(deph>o.deph){
            return 1;
        }else if(deph==o.deph){
            return 0;
        }else{
            return -1;
        }
    }

    public Point getPosition(){
        return position;
    }
    public void setPosition(Point p){
        position = new Point(p);
    }
    public abstract void translate(Point p);
    public abstract Point[] getBoundingBox();
    public abstract void draw();

    public double getPrecision() {
        return precision;
    }

    public void setPrecision(double precision) {
        this.precision = precision;
    }

    public char getCharToDraw() {
        return charToDraw;
    }

    public void setCharToDraw(char charToDraw) {
        this.charToDraw = charToDraw;
    }

    protected boolean isInsideScene(int x, int y){
        if(x<0 || y<0 || x>=sceneWidth || y>=sceneHeight){
            return false;
        }

        return true;
    }

    public int getDeph() {
        return deph;
    }

    public void setDeph(int deph) {
        this.deph = deph;
    }


}
