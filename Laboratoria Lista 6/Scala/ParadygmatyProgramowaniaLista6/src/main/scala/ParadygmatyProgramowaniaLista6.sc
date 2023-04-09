import scala.collection.mutable
import scala.collection.mutable.HashMap
//Zadanie 1

val stirling:(Int=>Int=>Int) = (n:Int)=>(m:Int)=>  (n,m) match
  case (_,1)=> 1
  case (n,m) if n==m => 1
  case (_,_)=>stirling(n-1)(m-1) + m*stirling(n-1)(m)

val hash_stirling:mutable.HashMap[(Int,Int),Int] =mutable.HashMap()


val memoized_stirling: (mutable.HashMap[(Int,Int),Int]=>Int=>Int=>Int) = (hash_table:mutable.HashMap[(Int,Int),Int])=>(n:Int)=>(m:Int)=>
  (n,m) match
    case (_,1)=>1
    case (h1,h2) if h1==h2 =>1
    case _ => {
      (hash_table.get((n, m))) match
        case None => val result = memoized_stirling(hash_table)(n-1)(m-1) + m*memoized_stirling(hash_table)(n-1)(m); hash_table.update((n,m),result); result
        case Some(result) => result
    }

stirling(10)(4)

//Zadanie 2

memoized_stirling(hash_stirling)(5)(4)

def fib(n:BigInt):BigInt = if n == 0 then 0 else if n==1 then 1 else fib(n-1)+fib(n-2)

val fib2:(Int=>Int)= (n:Int) => if n == 0 then 0 else if n==1 then 1 else fib2(n-1)+fib2(n-2)

val hash_fib:mutable.HashMap[Int,Int]=mutable.HashMap()

def make_memoize[A,B](hash_table:mutable.HashMap[A,B])(f:A=>B)(args:A) = hash_table.get(args) match
  case Some(result) => result
  case None => val result = f (args); hash_table.update(args,result); result

val memoized_fib = make_memoize(hash_fib)(fib2)

memoized_fib (50)
memoized_fib (43)

//fib2 (49)


//memoized_fib (49)
//memoized_fib (49)

//Zadanie 3

lazy val lazy_value = fib(40)
val unfreeze = lazy_value
