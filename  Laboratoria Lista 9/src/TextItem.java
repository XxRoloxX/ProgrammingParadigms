import java.awt.*;
import java.awt.image.BufferedImage;

public class TextItem extends Item {

    protected String text;
    protected int fontSize;
    protected int width;
    protected int height;

    protected int treshhold;

    public TextItem(Point position, String text, int fontSize,int width, int height){
        super(position);
        this.text = text;
        this.fontSize = fontSize;
        this.width=width;
        this.height=height;
        treshhold = GeometryUtils.DEFAULT_TRESHHOLD;
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
        boundingBox[1] = new Point(position.getX()+width, position.getY());
        boundingBox[2] = new Point(position.getX(),position.getY()+height);
        boundingBox[3] = new Point(position.getX()+width, position.getY()+height);

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

        drawAdvanced(treshhold,true);

    }
    public void drawAdvanced(int treshhold, boolean inverse){
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics graphics = image.getGraphics();
        graphics.setFont(new Font("Helvetica", Font.PLAIN, fontSize));
        Graphics2D graphics2D = (Graphics2D)graphics;
        graphics2D.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        graphics2D.drawString(text, 0, height/2);

        StringBuilder stringBuilder = new StringBuilder();

        for (int y = 0; y < height; y++) {

            for (int x = 0; x < width; x++) {
                if(inverse){
                    if(image.getRGB(x, y) < treshhold){
                        scene.getSceneField()[y+position.getY()][x+position.getX()] = charToDraw;
                    }
                }else{
                    if(image.getRGB(x, y) > treshhold){
                        scene.getSceneField()[y+position.getY()][x+position.getX()] = charToDraw;
                    }
                }

            }

        }

    }
    public void translate(Point p){
        position.translate(p);
    }


}
