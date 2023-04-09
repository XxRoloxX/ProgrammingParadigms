public class Point3D extends Point {
    protected double z;

    char charToDraw;

    public Point3D(int x,int y, int z){
        super(x,y);
        this.z=z;
    }
    public Point3D(Point3D otherPoint){
        this.x=otherPoint.x;
        this.y=otherPoint.y;
        this.z=otherPoint.z;
        this.charToDraw= otherPoint.charToDraw;
    }

    public int getZ() {
        return (int)Math.round(z);
    }

    public void setZ(int z) {
        this.z = z;
    }
    public void rotateY(double angle){

        double previousRadius = Math.sqrt(Math.pow(x,2)+Math.pow(z,2));
        double newRadius;
        double factor;
        /*
        z = x*Math.sin(angle) + z*Math.cos(angle);
        x = x*Math.cos(angle) - z*Math.sin(angle);
        */
        double oldZ = z;

        z = z*Math.cos(angle)-x*Math.sin(angle);
        x = x*Math.cos(angle) + oldZ*Math.sin(angle);

        newRadius = Math.sqrt(Math.pow(x,2)+Math.pow(z,2));

        factor = previousRadius/newRadius;

        //x*=(factor);
        //z*=(factor);

    }
    public void rotateZ(double angle){
        double previousRadius = Math.sqrt(Math.pow(x,2)+Math.pow(y,2));
        double newRadius;
        double factor;
        /*
        z = z*Math.cos(angle)-y*Math.sin(angle);
        y = y*Math.cos(angle) + z*Math.sin(angle);
         */
        double oldX = x;

        x = x*Math.cos(angle) - y*Math.sin(angle);
        y = oldX*Math.sin(angle) + y*Math.cos(angle);

        newRadius = Math.sqrt(Math.pow(x,2)+Math.pow(y,2));

        factor = previousRadius/newRadius;

       // y*=(factor);
        //x*=(factor);
    }
    public void rotateX(double angle){
        double previousRadius = Math.sqrt(Math.pow(z,2)+Math.pow(y,2));
        double newRadius;
        double factor;
        /*
        z = z*Math.cos(angle)-y*Math.sin(angle);
        y = y*Math.cos(angle) + z*Math.sin(angle);
         */

        double oldY = y;

        y = y*Math.cos(angle) - z*Math.sin(angle);
        z = oldY*Math.sin(angle) + z*Math.cos(angle);

        newRadius = Math.sqrt(Math.pow(z,2)+Math.pow(y,2));

        factor = previousRadius/newRadius;

        //System.out.println(factor);

        //y*=(factor);
       // z*=(factor);
    }
    public void rotateX90deg(){
        double tmp =y;
        y=(-z);
        z=tmp;

    }
    public void rotateY90deg(){
        double tmp = x;
        x=z;
        z=(-tmp);

    }

    public void rotateZ90deg(){
        double tmp = x;
        x=(-y);
        y=tmp;
    }

    public char getCharToDraw() {
        return charToDraw;
    }

    public void setCharToDraw(char charToDraw) {
        this.charToDraw = charToDraw;
    }
    public void translate(Point3D p){
        this.x += p.x;
        this.y += p.y;
        this.z += p.z;
    }
    public String toString(){
        return "("+x+", "+y+", "+z+")";
    }
}
