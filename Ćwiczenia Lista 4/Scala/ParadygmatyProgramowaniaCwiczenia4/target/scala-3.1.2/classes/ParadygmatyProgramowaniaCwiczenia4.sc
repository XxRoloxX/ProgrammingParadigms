
//Zadanie 3
/*
enum Bt[+A]:
  case Node[+A](x:(Bt[A],Bt[A],A)) extends Bt[A]
  case Empty extends Bt[Nothing]
*/

sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](value: A, left: BT[A], right: BT[A]) extends BT[A]


def breathBT[A](firstElement:BT[A]):List[A] = {
  def breathBTAccum(queue: List[BT[A]], accum: List[A]): List[A] = queue match
  case Node(v, l, r) :: t => breathBTAccum(t ::: List(l, r), v :: accum)
  case Empty :: t => breathBTAccum(t, accum)
  case List() => accum.reverse

  breathBTAccum(List(firstElement), List())
}

val tt:BT[Int] = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Node(7, Empty, Empty)));
val tt2:BT[Int] = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))

breathBT(tt)

//Zadanie 4
def preOrder[A](startNode:BT[A]):List[A] =
  def preOrderHelp (node:BT[A],accum:List[A]):List[A] = node match
  case Empty=>accum
  case Node(v,l,r)=> v::preOrderHelp(l,preOrderHelp(r,accum))

  preOrderHelp(startNode,List())


preOrder(tt)

def innerPathLength[A](startNode:BT[A]):Int = {
  def innerPathLengthAccum[A](node: BT[A], accum: Int,level:Int):Int = node match
    case Empty=>accum
    case Node(v,l,r)=>level+innerPathLengthAccum(l,innerPathLengthAccum(r,accum,level+1),level+1)

    innerPathLengthAccum(startNode,0,0)
}
def outerPathLength[A](startNode:BT[A]):Int = {
  def outerPathLengthAccum[A](node: BT[A], accum: Int,level:Int):Int = node match
    case Empty=>accum+level
    case Node(v,l,r)=>outerPathLengthAccum(l,outerPathLengthAccum(r,accum,level+1),level+1)

  outerPathLengthAccum(startNode,0,0)
}


outerPathLength(tt2)

innerPathLength(tt2)

//Zadanie 5
sealed trait Graphs[A]
case class Graph[A](succ:A=>List[A]) extends Graphs[A]

val g = Graph((i: Int) => {i match
 case 0 => List(3)
 case 1 => List(0, 2, 4)
 case 2 => List(1)
 case 3 => Nil
 case 4 => List(0, 2)
 case n =>
  throw new Exception(s"Graph g: node $n doesn't exist")})


def dephSearch[A](g:Graph[A],startNode:A):List[A] = {
  def dephSearchHelp(visited: List[A], stack: List[A]):List[A] = stack match
    case List() => List()
    case h::t if visited contains h => dephSearchHelp(visited,t)
    case h::t => h::dephSearchHelp(h::visited,(g succ h):::t)
dephSearchHelp(List(),List(startNode))
}

dephSearch(g,4)

