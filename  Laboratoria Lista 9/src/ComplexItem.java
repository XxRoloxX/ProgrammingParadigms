import java.util.ArrayList;
import java.util.List;

public class ComplexItem extends Item {
    private ArrayList<ItemInterface> children;

    public ComplexItem(){
        children = new ArrayList<>();
    }

    public void addItem(ItemInterface newItem){
        children.add(newItem);
    }

    @Override
    public void translate(Point p) {
        for(int i=0;i<children.size();i++){
            children.get(i).translate(p);
        }
    }

    @Override
    public Point[] getBoundingBox() {

        int left;
        int right;
        int top;
        int bottom;

        if(children.size()==0){
            return new Point[0];
        }

        Point[] boundingBox = children.get(0).getBoundingBox();
        left = boundingBox[0].getX();
        top = boundingBox[0].getY();
        right = boundingBox[3].getX();
        bottom = boundingBox[3].getY();

        for(int i=1;i<children.size();i++){

            boundingBox = children.get(i).getBoundingBox();

            if(boundingBox[0].getX()<left){
                left = boundingBox[0].getX();
            }
            if(boundingBox[0].getY()<top){
                top = boundingBox[0].getY();
            }
            if(boundingBox[3].getX()>right){
                right = boundingBox[3].getX();
            }
            if(boundingBox[3].getY()>bottom){
                bottom = boundingBox[3].getY();
            }
        }
        boundingBox[0] = new Point(left,top);
        boundingBox[1] = new Point(right,top);
        boundingBox[2] = new Point(left,bottom);
        boundingBox[3] = new Point(right,bottom);

        return boundingBox;
    }
    public void setScene(Scene newScene){

        scene= newScene;

        for(int i=0;i<children.size();i++){
            children.get(i).setScene(newScene);
        }

    }

    @Override
    public void draw() {
        for(int i=0;i<children.size();i++){
            children.get(i).draw();
        }
    }
    public List<ItemInterface> getChildren(){
        return children;
    }

}
