
"Zadanie 1"
1*3*9 + 10 -2

5*1 -800 +999

100.0/60.0 + 2

"Zadanie 2"

"Niemutowalna zmienna"
val x = 99

x=80
"Mutowalna zmienna"
var y =99

y=80

"Zadanie 3"

val sqr = (x:Int) => x*x

sqr(10)

sqr(3)

val ratio =(x:Float, y:Float) => if y==0.0 then throw new Exception("argument rowny zero")
else x/y

ratio(10.0,2.0)

ratio(2.0,0.0)

val max = (x:Int,y:Int) => if x>y then x else y

max(10,2)

max(1,5)

"Zadanie 4"

val is_even = (x:Int) => if x%2 ==0 then true else false

val is_bigger_than_ten= (x:Int) => if(x>10) then true else false

val is_smaller_than_thousand = (x:Int) => if(x<1000) then true else false

def check (test: (Int=>Boolean),x: Int) = if test(x) then x else 0

def return_if_test_passed (test: Int=>Boolean, x:Int):Int =  if test(x) then x else 0;

def sum_if_true(test: (Int => Boolean), x: Int, y:Int, z:Int):Int =
  return_if_test_passed(test, x) + return_if_test_passed(test,y)+ return_if_test_passed(test,z);

sum_if_true(is_even, 1,2,3)

sum_if_true(is_bigger_than_ten, 11, 2, 30)

sum_if_true(is_smaller_than_thousand, 70, 20, 2000)

"Zadanie 5"

val friends = List("Paweł","Nikodem","Karolina","Dominik","Krzysztof","Julia")
val new_friend = "Wojtek"

val update_friend_list = (friends:List[String], new_friend: String) => List(new_friend):::friends

update_friend_list(friends, new_friend)