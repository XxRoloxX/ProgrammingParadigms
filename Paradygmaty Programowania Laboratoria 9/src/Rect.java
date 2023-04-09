public class Rect extends Shape {
    private int width;
    private int height;

    public Rect(){
        width=0;
        height=0;
    }
    public Rect(Point position,boolean filled,int width, int height){
        super(filled,position);
        this.width = width;
        this.height = height;
    }

    @Override
    public Point[] getBoundingBox() {

        Point[] boundingBox = new Point[4];
        boundingBox[0] = new Point(position);
        boundingBox[1] = new Point(position.getX() + width-1, position.getY() );
        boundingBox[2] = new Point(position.getX(), position.getY()+height-1);
        boundingBox[3] = new Point(position.getX()+width-1, position.getY()+height-1);

        return boundingBox;

    }

    @Override
    public void draw() {
        Point[] boundingBox = getBoundingBox();
        for(int i=boundingBox[0].getY();i<=boundingBox[2].getY();i++) {
            for (int j = boundingBox[0].getX(); j <= boundingBox[1].getX(); j++) {

                if (getFilled() == true || j==boundingBox[0].getX() || j == boundingBox[1].getX() || i==boundingBox[0].getY() || i==boundingBox[2].getY()) {
                    if(isInsideScene(j,i)){
                        scene.getSceneField()[i][j] = charToDraw;
                    }
                }
            }
        }
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public void translate(Point o){
        position.translate(o);
    }
}
