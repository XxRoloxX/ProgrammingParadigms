
public class Main {
    static boolean isEqual1(int m, int n){return m == n;}
    static boolean isEqual2(Integer m, Integer n){return m == n;}
    public static void main(String[] args) {
        //Zadanie 3
        /*
            System.out.println(isEqual1(500,500));
            System.out.println(isEqual2(500,500));
        */

        //Zadanie 4
        /*
        String s1 = "foo";
        String s2 = "foo";
        System.out.println(s1 == s2);
        System.out.println(s1.equals(s2));
        String s3 = new String("foo");
        System.out.println(s1 == s3);
        System.out.println(s1.equals(s3));
        */

        //Zadanie 5

        int[] ints = {1,2,3};
        for(int i : ints) {
            System.out.println(i); i = 0;
        }
        for(int i : ints)
            System.out.println(i);
        int[] ints2 = ints;
        for(int i=0; i<ints2.length; i++) {
            System.out.println(ints2[i]); ints2[i] = -1;
        }
        for(int i : ints) System.out.println(i);



    }
}