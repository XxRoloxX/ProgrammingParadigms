import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class Scene {
    private int width;
    private int height;

    private char[][] sceneField;
    private ArrayList<Item> items;

    public Scene(int width, int height){
        this.width=width;
        this.height=height;
        sceneField = new char[height][width];
        items = new ArrayList<>();

        for(int i=0;i<height;i++){
            for(int j=0;j<width;j++){
                sceneField[i][j]=' ';
            }
        }

    }

    public void addItem(Item newItem){
        newItem.setScene(sceneField,height,width);
        items.add(newItem);
    }
    public void removeItem(Item item){
        items.remove(item);
    }

    public void printScene(){

        StringBuilder result = new StringBuilder();
        /*
        for(int i=0;i<height;i++){
            for(int j=0;j<width;j++){
                System.out.print(sceneField[i][j]);
                System.out.print("  ");
            }
            System.out.println();
        }

         */

        for(int i=0;i<height;i++){
            for(int j=0;j<width;j++){
               result.append(sceneField[i][j]);
               result.append("  ");
            }
           result.append('\n');
        }
        System.out.println(result);
    }

    public void draw(){

        Collections.sort(items);

        for(int i=0;i<items.size();i++){
            items.get(i).draw();
        }

        printScene();
    }
    public void clearScene(){
        for(int i=0;i<height;i++){
            for(int j=0;j<width;j++){
                sceneField[i][j]=' ';
            }
        }
    }
    public void emptyScene(){
        items = new ArrayList<Item>();
    }
    public static void clearScreen() {
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }



}
