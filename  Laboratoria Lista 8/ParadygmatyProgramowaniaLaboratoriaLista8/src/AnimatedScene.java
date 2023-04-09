import java.awt.*;
import java.util.ArrayList;
import java.util.Iterator;

public class AnimatedScene {

    private Scene scene;
    private ArrayList<Video> videos;

    boolean animationFinished;

    ArrayList<Iterator<Item>>videoIterators;


    public AnimatedScene(int width, int height){
        scene = new Scene(width,height);
        videos = new ArrayList<>();
        animationFinished=false;
    }

     void addVideo(Video video){
        videos.add(video);
        scene.addItem(video.getNewFrame());
     }

     void addItem(Item item){
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

     void drawFrames() throws InterruptedException {


        while(!animationFinished){
            animationFinished=true;

            for(Video video: videos){
                if(video.getNewFrame()!=null){
                    animationFinished=false;
                }
            }


            ImageToASCII.clearScreen();
            scene.clearScene();
            scene.draw();
            Thread.sleep(10);

        }
     }







}
