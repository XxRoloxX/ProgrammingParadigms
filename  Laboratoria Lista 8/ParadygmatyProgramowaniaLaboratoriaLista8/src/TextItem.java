import java.awt.*;
import java.awt.image.BufferedImage;

public class TextItem extends Item {

    private String text;
    private int fontSize;


    public TextItem(){
        text="";
    }
    public TextItem(Point position, String text, int fontSize){
        super(position);
        this.text = text;
        this.fontSize = fontSize;
    }

    /*
    public String drawString(String text, String charToDraw, int imageWidth, int imageHeight, int fontSize, int xPosition, int yPosition){

        BufferedImage image = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB);
        Graphics graphics = image.getGraphics();
        graphics.setFont(new Font("Helvetica", Font.PLAIN, fontSize));
        Graphics2D graphics2D = (Graphics2D)graphics;
        graphics2D.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        graphics2D.drawString(text, xPosition, yPosition);

        StringBuilder stringBuilder = new StringBuilder();

        for (int y = 0; y < imageHeight; y++) {

            for (int x = 0; x < imageWidth; x++) {
                stringBuilder.append(image.getRGB(x, y) <= -16000000 ? " ": charToDraw);
            }
            stringBuilder.append("\n");

        }

        return stringBuilder.toString();

    }

     */

    @Override
    public Point[] getBoundingBox() {

        Point[] boundingBox = new Point[4];
        boundingBox[0] = new Point(position.getX(),position.getY());
        boundingBox[1] = new Point(position.getX()+text.length(), position.getY());
        boundingBox[2] = new Point(position.getX(),position.getY());
        boundingBox[3] = new Point(position.getX()+text.length(), position.getY());

        return boundingBox;

    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public void draw() {

        BufferedImage image = new BufferedImage(sceneWidth, sceneHeight, BufferedImage.TYPE_INT_RGB);
        Graphics graphics = image.getGraphics();
        graphics.setFont(new Font("Helvetica", Font.PLAIN, fontSize));
        Graphics2D graphics2D = (Graphics2D)graphics;
        graphics2D.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        graphics2D.drawString(text, position.getX(), position.getY());

        StringBuilder stringBuilder = new StringBuilder();

        for (int y = 0; y < sceneHeight; y++) {

            for (int x = 0; x < sceneWidth; x++) {
                if(image.getRGB(x, y) > -16000000){
                    sceneField[y][x] = charToDraw;
                }
            }

        }

    }
    public void translate(Point p){
        position.translate(p);
    }
}
