//Zadanie 1
class GenericCellImm[T] (val x: T)
// defined class GenericCellImm
class GenericCellImm[+T] (val x: T)
// defined class GenericCellImm
class GenericCellMut[T] (var x: T)
// defined class GenericCellMut
class GenericCellMut[+T] (var x: T)

//covariant type T occurs in contravariant position in type T of parameter x_=

//Ponieważ w argumencie konstruktora głownego podajemy var x to scala automatycznie tworzy setter, getter
//Jednoczenie ponieważ jest to definicja klasy kowariantnej to tworzy się setter x_(x:T) gdzie T jest kowariantne,a
//powinno być kontrawariantne, aby zachodziła relacja kowariancja klasy zgodnie z zasadą S<:S` (typ argumentu)
// i T`<T (typ zwracany)

//Nie możemy tego zrealizować ponieważ jedyny przypadek dla klasa moze miec pole modyfikowalne (czyli takie ktore ma setter i getter)
//jest wtedy kiedy typ jest inwariantny.

class GenericCellMut[+T] (x: T):
  private val xInner;
  def x_[S>:T](newX:S) = x=newX;



class GenericCellMut[-T] (private var x: T)
//Tutaj jest analogiczne sytuacja jak dla typu kowariantnego, tj. problem zachodzi dla
//automatycznie generowanego gettera x o  definicji def x():T,jednoczenie wiemy, że
// zwracany typ powinien być podtypem T, a nie tak jak sugeruje kontrawariantność
//nadtypem T


//Zadanie 2
abstract class Sequence[+A]:
  def append[S>:A](x: Sequence[S]): Sequence[A]

//Sequence[A] jest typem kowariantym, tzn dla S<:T Sequence[S]<:Sequence[T], a jednoczenie w metodzie chcemy zaaplikować
//argument ktory ma być kowariantny (zgodnie z def. Sequence[A]), ale jednoczesnie wszystkie argumenty przekazywane do metod tej klasy
//muszą być kontrawariantne, a wiemy, że Sequence[A] jest kowariantne (Z zasady, że S<:S` i T`<:T aby  S`=>T` <: S=>T) Wiec nie moze byc typem argumentu
//Tutaj zachodzi, sprzecznosc. Nie ma problemu z typem zwracanym, ponieważ wiemy, wartości,
// które funkcja ma zwracac maja byc kowariantne czyli, tak jak zdefiniowalismu Sequence[+A]

//Inaczej
//Argumenty funkcji powinny byc kontrawariante, a wartosci zwracane kowariantne, poniważ zdeficniowalismy Sequence[+A] jako typ
//kowariantny to nie może on być argumencie funkcji, może za to być zwracany.

//Zadanie 3

class UnderflowException(msg: String) extends Exception(msg)


class MyQueue[+T] private(private val pair: (List[T], List[T])):

  private def normalize[S >: T](stack: MyQueue[S]) = stack.pair match
    case (List(), ending) => new MyQueue(ending.reverse, List())
    case (_, _) => stack

  def enqueue[S >: T](x: S) = pair match
    case (beginning, ending) => normalize(new MyQueue(beginning, x :: ending));

  def dequeue() = pair match
    case (List(), _) => throw new UnderflowException("Queue is empty!")
    case (begHead :: begTail, ending) => normalize(new MyQueue(begTail, ending))

  def first() = pair match
    case (List(), _) => throw new UnderflowException("Queue is empty!")
    case (begHead :: begTail, ending) => begHead

  def isEmpty(): (Boolean) = pair match
    case (List(), _) => true
    case (_, _) => false

object MyQueue:
    def apply[T](xs: T*) = new MyQueue[T]((xs.toList,List()))
    def empty[T] = new MyQueue[T](List(),List())



var queueOfNumbers = MyQueue[Number](1.2,5.5);
var queueOfIntegers = MyQueue[Integer](1,2,3);


queueOfNumbers = queueOfIntegers;

queueOfNumbers = queueOfNumbers.enqueue(2.5);
queueOfNumbers.first();
queueOfNumbers = queueOfNumbers.dequeue()
queueOfNumbers.first();
queueOfNumbers = queueOfNumbers.dequeue()
queueOfNumbers.first();
queueOfNumbers = queueOfNumbers.dequeue()
queueOfNumbers.first();
/*
class copyFunction[-S,+T]

object copyFunction:
  def copy[S,T](src:List[S],dest:List[T]) = src.foreach(el:S =>
     { dest.update()}
  )
*/
//Zadanie 3

def copy[T](src:scala.collection.mutable.Seq[T],dest:scala.collection.mutable.Seq[T]) =
{var index =0; src.foreach(el=> {dest.update(index,el); index+=1})}


val list1 = scala.collection.mutable.Seq(1,2,3);

val list2 = scala.collection.mutable.Seq(4,5,6);


copy(list1,list2);

list2