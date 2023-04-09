public class ItemDecorator implements ItemInterface{

    protected ItemInterface decoratedItem;

    public ItemDecorator(ItemInterface decoratedItem){
        this.decoratedItem=decoratedItem;
    }


    @Override
    public int compareTo(Item o) {
        return decoratedItem.compareTo(o);
    }

    @Override
    public Point getPosition() {
        return decoratedItem.getPosition();
    }

    @Override
    public void setPosition(Point p) {
        decoratedItem.setPosition(p);
    }

    @Override
    public void translate(Point p) {
        decoratedItem.translate(p);
    }

    @Override
    public Point[] getBoundingBox() {
        return decoratedItem.getBoundingBox();
    }

    @Override
    public void draw() {
        decoratedItem.draw();
    }

    @Override
    public double getPrecision() {
        return decoratedItem.getPrecision();
    }

    @Override
    public void setPrecision(double precision) {
        decoratedItem.setPrecision(precision);
    }

    @Override
    public char getCharToDraw() {
        return decoratedItem.getCharToDraw();
    }

    @Override
    public void setCharToDraw(char charToDraw) {
        decoratedItem.setCharToDraw(charToDraw);
    }

    @Override
    public boolean isInsideScene(int x, int y) {
        return decoratedItem.isInsideScene(x,y);
    }

    @Override
    public int getDeph() {
        return decoratedItem.getDeph();
    }

    @Override
    public void setDeph(int deph) {
        decoratedItem.setDeph(deph);
    }

    @Override
    public Scene getScene() {
        return decoratedItem.getScene();
    }
    @Override
    public void setScene(Scene newScene) {
         decoratedItem.setScene(newScene);
    }


}
