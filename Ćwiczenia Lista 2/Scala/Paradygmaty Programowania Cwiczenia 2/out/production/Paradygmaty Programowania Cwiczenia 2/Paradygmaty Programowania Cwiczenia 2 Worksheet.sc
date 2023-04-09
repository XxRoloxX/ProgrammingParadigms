import scala.annotation.tailrec


//Zadanie 1
def evenR(n:Int):Boolean ={ def oddR(n:Int):Boolean = if n==0 then false else evenR(n-1)
  if n==0 then true else oddR(n-1) }

evenR(6)


//Zadanie 2
def fib(n:BigInt):BigInt = if n == 0 then 0 else if n==1 then 1 else fib(n-1)+fib(n-2)

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)

//fib(42)

@tailrec
def fibIter(n:Int,iter:Int,accum1:BigInt,accum2:BigInt):BigInt = {if n==0 then 0
else if n==iter then accum1 else fibIter(n,iter+1,accum2, accum1+accum2)}

def fibTail(n:Int):BigInt = fibIter(n,0,0,1)

fibTail(0)
fibTail(1)
fibTail(2)
fibTail(3)
fibTail(4)
fibTail(5)
fibTail(6)
fibTail(7)
fibTail(8)

//fibTail(42)


//Zadanie 3
val absolute_value = (number:Double) => if number<0 then number*(-1.0) else number

val next_approx = (number:Double, result:Double) => result + (number/(result*result) - result)/3.0

val relative_error = 0.000000000000001

val is_less_than_relative_error = (number: Double, result: Double, error: Double) => if absolute_value(result*result*result - number)<error*absolute_value(number) then true else false


//Funkcja
val cubeSquareIterFunc :((Double,Double)=>Double)= (number:Double, result:Double)=> if is_less_than_relative_error(number, result,relative_error) then result else cubeSquareIterFunc(number, next_approx(number, result))

val cubeSquareTailFunc = (number: Double) => if number>1 then cubeSquareIterFunc(number, number/3.0)
else cubeSquareIterFunc(number, number)

cubeSquareTailFunc(1000.0)

//Metoda
def cubeSquareIter(number:Double, result:Double):Double= if is_less_than_relative_error(number, result,relative_error) then result else cubeSquareIter(number, next_approx(number, result))

def cubeSquareTail(number: Double):Double = if number>1 then cubeSquareIter(number, number/3.0)
else cubeSquareIter(number, number)

cubeSquareTail(27000.0)

//Zadanie 4 a)

val a = List(-2,-1,0,1,2)

val List(_,_,x,_,_) = a

//Zadanie 4 b)
val b = List((1,2),(0,1))

val List((_,_),(x,_)) = b
