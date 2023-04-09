public class Main {
    public static void main(String[] args) {
        MyQueue<Integer> queue = new MyQueueCyclic<Integer>(3);

        try {
            queue.enqueue(1);
            queue.enqueue(2);
            queue.enqueue(3);
            System.out.println("Pierwszy element: "+queue.first());
            queue.dequeue();
            System.out.println("Pierwszy element: "+queue.first());
            queue.dequeue();
            System.out.println("Pierwszy element: "+queue.first());
            queue.dequeue();
            queue.enqueue(1);
            queue.enqueue(2);
            System.out.println("Pierwszy element: "+queue.first());
            queue.dequeue();
            System.out.println("Pierwszy element: "+queue.first());
            queue.dequeue();


        }catch(FullException | EmptyException e){
            System.out.println("Zlapano wyjatek: "+e);
        }

        System.out.println("Hello world!");
    }
}