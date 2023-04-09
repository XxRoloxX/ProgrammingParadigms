import scala.annotation.tailrec

//Zadanie 1

def repeat[A](llist:LazyList[A],n:Int):LazyList[A] = {
  def repeat_help[A](inner_llist: LazyList[A], left: Int): LazyList[A] = inner_llist match
    case head #:: ltail if left > 1 => head#::repeat_help(inner_llist, left - 1)
    case head #:: ltail => head#::repeat_help(ltail, n)
    case LazyList() => LazyList()

  repeat_help(llist, n)
}

val naturals:(Int=>LazyList[Int]) =(n:Int) => n #:: naturals(n+1)

val resolution = repeat(naturals (0), 3).take(10).toList

repeat(LazyList.from(1).take(15),3).toList

//Zadanie 2
@tailrec
def fibIter(n:Int,iter:Int,accum1:Int,accum2:Int):Int = {if n==0 then 0
else if n==iter then accum1 else fibIter(n,iter+1,accum2, accum1+accum2)}

def fibTail(n:Int):Int = fibIter(n,0,0,1)


val lfib:LazyList[Int] = {
  def lfib_start (n: Int):LazyList[Int] = fibTail(n) #:: lfib_start(n + 1);
  lfib_start(0)
}

lfib.take(15).toList

//Zadanie 3

sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem:A, left:()=>lBT[A],right:()=>lBT[A]) extends lBT[A]


def lBreadth[A](ltree:lBT[A]):LazyList[A] = {
  def lBreadthQueue[A](queue: List[lBT[A]]):LazyList[A] = queue match
    case LNode(elem:A,left,right)::t => elem#::lBreadthQueue(t:::List(left(),right()))
    case LEmpty::t => lBreadthQueue(t)
    case _ => LazyList()

    lBreadthQueue(List(ltree))
}


def lTree(n:Int):lBT[Int] = LNode(n,() => lTree(2*n),()=>lTree(2*n+1))

lBreadth(lTree(1)).take(20).toList

lBreadth(LEmpty).take(20).toList


