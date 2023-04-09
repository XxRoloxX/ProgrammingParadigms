import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

public class ImageToASCII extends Item {

    public static final int DEFAULT_THRESHOLD =  -7500000;
    private BufferedImage image;
    private InputStream fileHandler;

    private int threshold;

    //String ASCIIImage;

    private char [][] ASCIIImage;

    private int width;
    private int height;
    public ImageToASCII(Point position){
        super(position);
        width=0;
        height=0;
        fileHandler=null;
        threshold= DEFAULT_THRESHOLD;
    }
    public ImageToASCII(){
        super();
        width=0;
        height=0;
        fileHandler=null;
        threshold= DEFAULT_THRESHOLD;
    }
    public void copyASCIIImage(ImageToASCII other){
        ASCIIImage=other.ASCIIImage;
        width = other.width;
        height = other.height;
        position = other.position;
        //image = other.image;
        //fileHandler = other.fileHandler;
        //sceneWidth = other.sceneWidth;
        //sceneHeight = other.sceneHeight;
        //sceneField = other.sceneField;
        charToDraw = other.charToDraw;
        threshold = other.threshold;
    }

    @Override
    public Point[] getBoundingBox() {
        Point[] boundingBox = new Point[4];
        boundingBox[0] = new Point(position);
        boundingBox[1] = new Point(position.getX()+width,position.getY());
        boundingBox[2] = new Point(position.getX(),position.getY()+height);
        boundingBox[3] = new Point(position.getX()+width,position.getY()+height);

        return boundingBox;
    }

    @Override
    public void draw() {

        //createASCII();
        for(int i=0;i<height;i++){
            for(int j=0;j<width;j++){

                if(isInsideScene(position.getX()+j,position.getY()+i)){
                    scene.getSceneField()[position.getY()+i][position.getX()+j]=ASCIIImage[i][j];
                }

            }
        }
    }

    public boolean openFile(String filePath){

        try {
            fileHandler = new FileInputStream(filePath);
           image = ImageIO.read(fileHandler);
           width = image.getWidth();
           height = image.getHeight();

           ASCIIImage = new char[height][width];
           //createASCII();
           //System.out.println("Typ:"+ image);
        }catch(IOException e){
            return false;
        }
        return true;

    }
    public static void clearScreen() {
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }

    public void createASCII(){
        //StringBuilder result = new StringBuilder();

        for(int i=0;i<height;i++){
            for(int j=0;j<width;j++){
                ASCIIImage[i][j] = image.getRGB(j,i) <=  threshold? ' ': '*';
                //-13005000
            }
        }

        if (fileHandler != null) {
            try {
                fileHandler.close();
            } catch (IOException ex) {
                System.out.println("ERROR closing image input stream: "+ex.getMessage()+ ex);
            }
        }
        fileHandler=null;
        image=null;

    }


    public void printASCIIImage(){
       System.out.println(ASCIIImage);
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public void translate(Point o){
        position.translate(o);
    }

    public void setThreshold(int threshold) {
        this.threshold = threshold;
    }
}
