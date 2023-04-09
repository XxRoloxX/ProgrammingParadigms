public abstract class Shape extends Primitive {
    protected boolean filled;

    public Shape(){
        this.filled=filled;
        this.position = new Point();
    }
    public Shape(boolean filled, Point position){
        this.filled=filled;
        this.position = position;
    }
    public boolean getFilled(){
        return filled;
    }
    public void setFilled(boolean filled){
        this.filled=filled;
    }

}
