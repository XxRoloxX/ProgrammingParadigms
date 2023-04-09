import java.util.ArrayList;

public class MyQueueCyclic<E> implements MyQueue<E>{

    private ArrayList<E> array;
    private int read;
    private int write;

    public MyQueueCyclic(int initialCapacity){
        read=0;
        write=0;
        array = new ArrayList<>(initialCapacity+1);
        for(int i=0;i<initialCapacity+1;i++){
            array.add(null);
        }
    }
    public void enqueue( E x ) throws FullException{
        if(isFull()){
            throw new FullException("Queue is Empty!");
        }
        else{
            System.out.println("Dodano element: "+x);
            array.set(write,x);
            write = (write+1)%array.size();
        }

    }
    public void dequeue( ) throws EmptyException{
        if(isEmpty()){
            throw new EmptyException("Queue is Empty!");
        }
        else{
            read=(read+1)%array.size();
        }
    }
    public E first( ) throws EmptyException{
        if(isEmpty()){
            throw new EmptyException("Queue is Empty!");
        }
        else{
            return array.get(read);
        }
    }
    public boolean isEmpty( ){
        return read==write;
    }
    public boolean isFull( ){
        if(read==0 && write==array.size()-1){
            return true;
        }
        else if(read>0 && write==read-1){
            return true;
        }
        else{
            return false;
        }
    }
}
