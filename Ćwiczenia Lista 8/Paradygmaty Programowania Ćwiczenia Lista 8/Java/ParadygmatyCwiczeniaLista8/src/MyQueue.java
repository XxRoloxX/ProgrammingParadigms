public interface MyQueue<E> {
    public void enqueue( E x ) throws FullException;
    public void dequeue( ) throws EmptyException;
    public E first( ) throws EmptyException;
    public boolean isEmpty( );
    public boolean isFull( );
}
