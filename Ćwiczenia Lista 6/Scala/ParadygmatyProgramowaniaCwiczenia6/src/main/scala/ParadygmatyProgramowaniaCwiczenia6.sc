
/*
def whileLoop0(condition:()=>Boolean)(expression:()=>Unit):Unit =
  if condition() then {expression() ;whileLoop0(condition)(expression)}

var count=(-5)

val condition = ()=> count<3

val operation: ()=>Unit = ()=>{println(count) ;  count+=1}
//val operation2: Unit = {println(count) ;  count+=1}
//operation2
//operation2

///operation()
///operation()
///println(count)

whileLoop0(condition)(operation)
*/

//Zadanie 1

def whileLoop1(condition:()=>Boolean)(expression:()=>Unit):Unit = {
  def whileLoop1Inner(condition:()=>Boolean)(expression:()=>Unit)(expressionEvaluated:Unit):Unit={
    if condition() then whileLoop1Inner(condition)(expression)(expression())
  }
  whileLoop1Inner(condition)(expression)(expression())
}

var count=(-5)

val condition = ()=> count<3
val operation2: ()=>Unit = ()=>{println(count) ;  count+=1}

val evalOperation: ()=>Unit = ()=>operation2()

//evalOperation()
//evalOperation()

def operation4:Unit = {println(count) ; (count+=1);()=> operation4}

whileLoop1(()=>count<3)(()=>operation4)

//Zadanie 2

def swap(tab:Array[Int])(i:Int)(j:Int):Unit =
   tab(j)={val copy = tab(i); tab(i)=tab(j);copy}

var testArray = Array(1,2,3,4,5)

swap(testArray)(1)(3)

testArray(1)
testArray(3)


def partition(tab:Array[Int])(left:Int)(right:Int):(Int,Int) = {
  {
    var i = left;
    var j = right;
    val pivot =  (tab((left+right)/2));
    while i <= j do
      while tab(i)<pivot do
        i+=1
      end while
      while tab(j)>pivot do
        j-=1
      end while
      if i<=j then {swap(tab)(i)(j); i+=1;j-=1;0}
    end while

    ;(i,j)
  }
}

def quick(tab:Array[Int])(left:Int)(right:Int):Unit = {
  if left<right then {
    {
      var (i,j) = partition(tab)(left)(right);
      if j-1<right-i then {quick(tab)(left)(j); quick(tab)(i)(right)}
      else  {quick(tab)(i)(right); quick(tab)(left)(j)};
    }
  }
}

val before = testArray
partition(testArray)(0)(4)
val result = testArray


def quicksort(tab:Array[Int]) = quick(tab)(0)(tab.length-1)

quicksort(testArray)

val after_sorting = testArray









