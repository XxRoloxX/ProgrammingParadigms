(** Zadanie 1 *)
let rec evenR n = if n==0 then true else oddR(n-1)
  and oddR n = if n==0 then false else evenR(n-1);;

  evenR(1000000000);;

(** Zadanie 2 *)

let rec fib n = if n=0 then 0 else if n=1 then 1 else fib (n-1) + fib (n-2);;

fib(82);;


let rec fibTail n = let rec fibIter(n,iter,accum1, accum2) = if n==0 then 0 
else if n==iter then accum1 
else fibIter(n,iter+1, accum2, accum1+accum2) in fibIter(n,0,0,1);;


fibTail(0);;
fibTail(1);;
fibTail(2);;
fibTail(3);;
fibTail(4);;
fibTail(5);;
fibTail(6);;
fibTail(7);;

fib(42);; 

fibTail(42);;

(** Zadanie 3 *)

let precision = 0.000000000000001;;

let absolute_value number = if number<0.0 then number*.(-1.0) else number;;

let is_relative_error_achived (number, result, precision) = if absolute_value(result*.result*.result -. number) <= precision*.absolute_value(number) then true else false;;

let next_approx (number,result) = result+.(number/.(result*.result)-.result)/.3.0

let rec cube_square_iter(number, result) = if is_relative_error_achived(number, result,precision) then result
else cube_square_iter(number, next_approx(number,result));;

let cube_square_tail(number) = if number<=1.0 then cube_square_iter(number,number) else cube_square_iter(number,number/.3.0);;

cube_square_tail(27000.0);;

(** Zadanie 4 a) *)

let a = [-1;-2;0;1;2];;
let b = [(1,2);(0,1)];;

let [_;_;x;_;_]= a;;

(** Zadanie 4 b) *)

let [(_,_);(x,_)] = b;;

(*Zadanie 5*)
let rec initSegment (list1, list2) = match (list1,list2) with
([],_)->true
|(h::t,[])->false
|(h1::t1,h2::t2) when h1=h2->initSegment(t1,t2)
|(_,_) -> false;;

initSegment([1],[]);;

let rec get_length list = if list = [] then 0 
else 1+ get_length(List.tl list);;
(*Zadanie 6*)
let reverse_order list = let rec reverse_order_accum(list, accum) = match list with
[]-> accum
| h::t-> reverse_order_accum(t,h::accum)
in reverse_order_accum(list,[]);;

let replaceNth (list,n,element) = let rec replaceNthaccum(list,n,element,accum) = match list with
[]->accum
|h::t when n=0 -> reverse_order(t)@(element::accum)
|h::t -> replaceNthaccum(List.tl list,n-1,element,h::accum)
in reverse_order(replaceNthaccum(list,n,element,[]));;

replaceNth([1;2;3;4;5;6;7],2,5);;