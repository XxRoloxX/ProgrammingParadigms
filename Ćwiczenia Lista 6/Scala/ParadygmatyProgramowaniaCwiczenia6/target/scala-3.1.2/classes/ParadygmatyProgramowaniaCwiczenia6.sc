
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

def whileLoop1(condition:()=>Boolean)(expression:Unit):Unit = {
  if condition() then whileLoop1(condition)(expression)
}

var count=(-5)

val condition = ()=> count<3
val operation2: ()=>Unit = ()=>{println(count) ;  count+=1}

val evalOperation: ()=>Unit = ()=>operation2()

evalOperation()
evalOperation()

def operation4:Unit = {println(count) ; (count+=1); ()=>operation4}

operation4
operation4
operation4
operation4


//whileLoop1(()=>count<3)(operation4)



/*
operation2()
operation2()
operation2()
*/




///whileLoop1(condition)(operation2)
