import scala.annotation.tailrec


def curry3[A,B,C,D](f1:((A,B,C)=>D))(x:A)(y:B)(z:C):D = f1(x,y,z)
def uncurry3[A,B,C,D](f1:(A=>B=>C=>D))(x:A,y:B,z:C):D = f1 (x) (y) (z)


val sumProdHelp= (tup:(Int,Int),x:Int) => tup match
  case (f,s) => (f+x,s*x)



def sumProd(list:List[Int]):(Int,Int) = list.foldLeft((0,1))(
  (tup:(Int,Int),x:Int) => tup match
  case (f,s) => (f+x,s*x))

sumProd(List(1,2,3,4,5))
sumProd(List(1))
sumProd(List())




val is_bigger = (x:Int) =>(y:Int) => x>y

val is_first_letter_bigger = (x:String)=>(y:String) => x.charAt(0) > y.charAt(0)


def reverse_order[A](list:List[A]):List[A] = {
  @tailrec
  def reverse_order_accum[A](list:List[A], accum:List[A]):List[A]
  = list match
    case List() => accum
    case h::t => reverse_order_accum(t, h::accum)

  reverse_order_accum(list,List())
}

val is_list =(list:List[Int]) => list match
  case List()=>false
  case h::t => t



is_list(List())
is_list(List(1))
is_list(List(1,2))



def put_in_correct_spot[A](list:List[A])(op:(A=>A=>Boolean))(element:A):List[A] = {
  @tailrec
  def put_in_correct_spot_accum(list: List[A])(element: A)(accum: List[A]):List[A] = {
    list match
      case List() => reverse_order(element::accum)
      case h :: t if op(h)(element) => (reverse_order(accum) ::: List(element) ::: list)
      case h :: t => put_in_correct_spot_accum(t)(element)(h :: accum)
  }
      put_in_correct_spot_accum(list)(element)(List())
}

put_in_correct_spot(List(1,2,3,4,5,6,10))(is_bigger)(2)
put_in_correct_spot(List(1,2,3,4,5,6,10))(is_bigger)(0)
put_in_correct_spot(List(1,2,3,4,5,6,10))(is_bigger)(22)
put_in_correct_spot(List(1,2,3,4,5,6,10))(is_bigger)(8)
put_in_correct_spot(List(0))(is_bigger)(1)






def insertSort[A](list:List[A])(op:(A=>A=>Boolean)) = {
  @tailrec
  def insertSortAccum[A](list: List[A])(op: (A => A => Boolean))(accum: List[A]): List[A]=
    list match
      case List() => accum
      case h::t => insertSortAccum(list.tail)(op)(put_in_correct_spot(accum)(op)(h))

  insertSortAccum(list)(op)(List())
}

insertSort(List(7,6,4,2,-1,4,5))(is_bigger)
insertSort(List(1,2,3,4,5,6,7))(is_bigger)
insertSort(List())(is_bigger)
insertSort(List(1))(is_bigger)
insertSort(List(2,2,2))(is_bigger)


insertSort(List("abc","acd","fds","ftg","bds","dsa"))(is_first_letter_bigger)


def merge [A](list1:List[A])(list2:List[A])(op:(A=>A=>Boolean)):List[A]= {
  @tailrec
  def mergeAccum(list1:List[A])(list2:List[A])(accum:List[A]):List[A]= (list1,list2) match
    case (h1::t1,h2::t2) if op (h1)(h2) => mergeAccum(list1)(t2)(h2::accum)
    case (h1::t1,h2::t2) => mergeAccum(t1)(list2)(h1::accum)
    case (List(),h2::t2) => reverse_order(accum):::list2
    case (h1::t2,List()) => reverse_order(accum):::list1
    case (_,_) => reverse_order(accum)

    mergeAccum(list1)(list2)(List())

}
merge(List(1,4,6,7,8,9,10,33))(List(0,4,7,12,33,55))(is_bigger)
merge(List(1))(List(0))(is_bigger)

def split_in_half[A](list:List[A]):(List[A],List[A]) = {
  @tailrec
  def split_in_half_accum(list: List[A])(accum: (List[A], List[A]))(n:Int):(List[A],List[A]) = list match
    case List() => (List(),List())
    case h::t if n==0 => val (p,s)=accum; (reverse_order(p),list)
    case h::t => val (p,s)=accum; split_in_half_accum(t)((h::p,s))(n-1)

    split_in_half_accum(list)(List(),List())(list.length/2)
}
split_in_half(List(1,2,3,4,5,6,7,7,8))
split_in_half(List(1,2,3,4,5,6,7,7))
split_in_half(List(1,2))
split_in_half(List(1))
split_in_half(List())




def mergeSort[A](list:List[A])(op:(A=>A=>Boolean)):List[A] = {
  def mergeSortAccum(list: List[A])(op: (A => A => Boolean)):List[A] = list match
    case List()=>List()
    case h::t if t==List() => List(h)
    case h::t => val (left,right) = split_in_half(list); merge(mergeSortAccum(left)(op))(mergeSortAccum(right)(op))(op)

    mergeSortAccum(list)(op)
}

mergeSort(List(1,2,3,4,5,6,7,8))(is_bigger)
mergeSort(List(6,5,4,3,2,1))(is_bigger)
mergeSort(List(2,4,2,1,5,6,85,4))(is_bigger)
mergeSort(List(2,2))(is_bigger)
mergeSort(List(2,1))(is_bigger)
mergeSort(List(2))(is_bigger)
mergeSort(List())(is_bigger)

mergeSort(List("abc","acd","fds","ftg","bds","dsa"))(is_first_letter_bigger)
