import java.awt.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

public class AnimatedScene {

    private Scene scene;
    private ArrayList<Video> videos;

    boolean animationFinished;

    double frameRate;

    ArrayList<Iterator<ItemInterface>>videoIterators;


    public AnimatedScene(int width, int height){
        scene = new Scene(width,height);
        videos = new ArrayList<>();
        animationFinished=false;
    }

     void addVideo(Video video){
        scene.addItem(video.getNewFrame());
        videos.add(video);

     }

     void addItem(ItemInterface item){
        scene.addItem(item);
     }
     /*
     private void createFrameIterators(){
         videoIterators = new ArrayList<>();

         for(Video video: videos){
             videoIterators.add(video.iterator());
         }
     }


      */

    void setFrameRate(double frameRate){
        this.frameRate = (1/frameRate)*1000;

    }

     void drawFrames() throws InterruptedException {
        Date date = new Date();

        long timeOfLastFrame = date.getTime();

        while(!animationFinished){
            //System.out.println(date.getTime()+", "+timeOfLastFrame+", "+frameRate);
            date = new Date();
            if(date.getTime()-timeOfLastFrame > frameRate){
                timeOfLastFrame = date.getTime();
                animationFinished = true;

                for (Video video : videos) {
                    if (video.getNewFrame() != null) {
                        animationFinished = false;
                    }
                }


                ImageToASCII.clearScreen();
                scene.clearScene();
                scene.draw();
                //Thread.sleep((int) frameRate);
            }

        }
     }







}
