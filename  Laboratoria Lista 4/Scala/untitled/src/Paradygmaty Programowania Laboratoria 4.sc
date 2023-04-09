import scala.annotation.tailrec
import scala.language.postfixOps
import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

def reverse_order[A](list:List[A]):List[A] = {
  @tailrec
  def reverse_order_accum[A](list:List[A], accum:List[A]):List[A] = list match
    case List() => accum
    case h::t => reverse_order_accum(t, h::accum)

  reverse_order_accum(list,List())
}

def read_int_table(table_size:Int):List[Int] ={
  @tailrec
  def read_int_table_accum(table_size:Int,accum:List[Int]):List[Int] =
  if table_size==0 then accum else read_int_table_accum(table_size-1,readInt()::accum)
  reverse_order(read_int_table_accum(table_size,List()))
}

//read_int_table(5);

//val read_int = readInt()

//reverse_order(List(1,2,3,4,6))

def get_length[A](list:List[A]):Int = if list == List() then 0
else 1+get_length(list.tail)

//Zadanie 1
def map[A,B](list:List[A])(f:(A=>B)):List[B] = {
@tailrec def map_accum[A,B](list:List[A])(f:(A=>B))(accum:List[B]):List[B] = list match
  case List()=> accum
  case h::t => map_accum(list.tail)(f)((f (list.head))::accum)
reverse_order(map_accum(list)(f)(List()))}

//map(List(1,2,3,4,5,6))(x=>x*x)

//Zadanie 2
@tailrec def filter_accum[A](list: List[A])(pred:(A=>Boolean))(accum: List[A]): List[A] = list match
  case List() => accum
  case h :: t if (pred (h)) => filter_accum(list.tail)(pred)((h) :: accum)
  case h :: t => filter_accum(list.tail)(pred)(accum)


def filter[A](list:List[A])(pred:(A=>Boolean)):List[A]= reverse_order(filter_accum(list)(pred)(List()))

val plus =(x:Int)=>(y:Int)=> x+y

//filter(List(1,2,3,4,5,6,7))(x=>x%2==0)
//Zadanie 3
@tailrec def reduce[A,B](list:List[A])(op:(A=>B=>B))(accum:B):B = list match
  case List()=>accum
  case h::t=>reduce(list.tail)(op)(op (list.head) (accum))

//reduce(List(1,2,3,4,5,6))(plus)(0)
/*
def split_string_into_list(text:String,char_for_split:Char, accum:List[String], is_after_char:Boolean):List[String] =
  if text.length ==0 then accum
  else if text.charAt(0)==char_for_split && text.length>=2 then split_string_into_list(text.substring(1),char_for_split,accum,true)
  else if text.charAt(0)==char_for_split && text.length<2 then accum
  else if is_after_char && text.length>=2 then split_string_into_list(text.substring(1),char_for_split,accum.concat(),false)

*/
//Zadanie 4
def average(list:List[Int]):Double = ((reduce(list)(plus)(0)):Double)/(get_length(list)):Double

//average(List(4,5,4,5))

val concate_if_space = (next_letter:String)=>(text:List[String])=> if text==List() then List(next_letter)
else if text.head == " " || next_letter ==" "
then next_letter::text else text

val is_not_space = (letter:String) => letter!=" "


//Zadanie 5
def string_to_list(text:String):List[String] = {
  @tailrec
  def string_to_list_accum(i:Int, accum:List[String]):List[String] =
  if i<0 then accum
else string_to_list_accum(i-1, text.charAt(i).toString::accum)
  string_to_list_accum(text.length-1, List())
}

def list_to_string(list:List[String]) = {
  @tailrec
  def list_to_string_accum(list:List[String], accum:String):String = list match
    case List()=> accum
    case h::t => list_to_string_accum(t,h+accum)

  list_to_string_accum(reverse_order(list),"")




}

//string_to_list("Zakład Ubezpieczeń Społecznych")
def acronym(text:String):String = list_to_string(
  reverse_order(
    filter(
      reduce(string_to_list(text))(concate_if_space)(List()))(is_not_space)))

acronym("Zakład Ubezpieczeń Społecznych")

acronym(readLine())

//Zadanie 6
def sum(list:List[Int]) = reduce(list)(plus)(0)


//def map_into_squares(list:List[Int]) = map(filter(list)(x=>x*x*x<=sum(list)))(x=>x*x)


def map_into_squares(list:List[Int]) = {val sum_of_list = sum(list);  map(filter(list)(x=>x*x*x<=sum_of_list))(x=>x*x)}

map_into_squares(List(1,2,3,4,5,1))

