/*
public class Test {
    int zawartość = 0;
    static void argNiemodyfikowalny(final Test zmienna) {
        zmienna.zawartość = 1;
        //Brak składowej
        zmienna = null;
    }
    static void argModyfikowalny(Test zmienna) {
        zmienna.zawartość = 1;
        zmienna = null;
    }
    public static void main(String[] args) {
        Test modyfikowalna = new Test();
        final Test niemodyfikowalna = new Test();
// tutaj wstaw instrukcje
    }
}
*/
public class Test {
    int zawartość = 0;
    static void argNiemodyfikowalny(final Test zmienna) {
        zmienna.zawartość = 1;
       // zmienna = null;  zmienna ma atrybut final co oznacza ze nie moze byc zmieniana (referencja nie może wskazywać gdzie indziej) Kopia referencji na ten sam obiekt
        
    }
    static void argModyfikowalny(Test zmienna) {
        zmienna.zawartość = 1;
        zmienna = null; //Tylko argument zmienia sie na null (oryginalna referencja pozostaje nie zmieniona, poniewaz do argumentu zostala przekazana KOPIA referencji)
    }
    public static void main(String[] args) {
        Test modyfikowalna = new Test();            //Modyfikowalna może być referencja na inne obiekty w przyszlosci
        final Test niemodyfikowalna = new Test();  //Final mówi o tym że niemodyfikowalna nie może być później "przepisana" jako referencja na inny obiekt
// tutaj wstaw instrukcje
        argNiemodyfikowalny(modyfikowalna);
        System.out.println(modyfikowalna.zawartość); //Zostaje poprawnie wyswietlona zawartosc

        argNiemodyfikowalny(niemodyfikowalna);
        System.out.println(niemodyfikowalna.zawartość);//Zostaje poprawnie wyswietlona zawartosc

        argModyfikowalny(modyfikowalna);
        System.out.println(modyfikowalna.zawartość);//Zostaje poprawnie wyswietlona zawartosc

        argModyfikowalny(niemodyfikowalna);
        System.out.println(niemodyfikowalna.zawartość);//Zostaje poprawnie wyswietlona zawartosc
    }
}