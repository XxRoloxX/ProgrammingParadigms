def flatten[A](xss:List[List[A]]):List[A] = if xss == Nil then Nil
else xss.head:::flatten(xss.tail)

flatten(List(List(1,2,3),List(5,6,7), List(8,9,10)))

def count[A](x:A,xs:List[A]):Int = if xs == List() then 0
else if xs.head == x then count(x,xs.tail)+1  else count(x,xs.tail)

count(1,List(1,2,3,4,5,6,3,5,6,3))

def replicate[A](x:A, n:Int):List[A] = if n==0 then List()
else if n>0 then List(x):::replicate(x,n-1)
else throw new Exception(s"ujemny argument: $n")

replicate("a", -9)

def sqrList(xs:List[Int]):List[Int] = if xs == List() then List()
else List(xs.head*xs.head):::sqrList(xs.tail)

sqrList(List(1,2,3,-4,19,0))


def palindrome[A](xs:List[A]):Boolean = if xs.length <=1 then true
else if xs.head != xs.reverse.head then false
else palindrome(xs.tail.reverse.tail)

def palindrome2[A](xs:List[A]):List[A] = xs.reverse == xs

palindrome(List(1,2,3,3,2,1))


def listLength[A](xs: List[A]):Int = if xs == List() then 0
else listLength(xs.tail)+1

listLength(List(2,3,1,2,3,4))
