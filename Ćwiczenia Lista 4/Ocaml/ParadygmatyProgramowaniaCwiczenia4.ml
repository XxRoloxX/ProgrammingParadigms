
(*Zadanie 1*)

(*f1: 'a->'b->'c->'a->'b->'c*)

let f1 x y z = x y z;;

(*Typ f1 'a->'b->'c->'d *)
(*Typ 'd: 'a->'b->'c*)
(*Zatem f1: ('a->'b->'c)->'a->'b->'c*)

(*
f2: 'a->'b->'c 
'c: 'd->('a::'e list) 
'b: 'e list
'e: 'a

f2: 'a->'a list->'d->'a list
*)



let f2 x y = function z->x::y



(*Zadanie 2*)

type b = (int*int*int);;

let rec f x = f x ;;


(*Zadanie 3*)
type 'a bt = Empty | Node of 'a*('a bt)*('a bt);;
type 'a graph = Graph of ('a->'a list);;

let breadthBT firstElement = 
  let rec breadthBTAccum queue accum = match queue with
   []-> List.rev accum
  |Empty::t->breadthBTAccum(t)(accum)
  |(Node (v,l,r))::t-> breadthBTAccum(t@[l;r])(v::accum)
in match firstElement with
  Empty->[]
 |Node(v,l,r)->breadthBTAccum [firstElement] [];;




let tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Node(7,Empty,Empty)));;
let tt2 = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty));;


 breadthBT tt2;;

(*Zadanie 4*)

let preOrder startNode = 
  let rec preOrderHelp node accum = match node with
  Empty -> accum
 | Node(v,l,r)->v::preOrderHelp(l) (preOrderHelp(r) accum)
in preOrderHelp startNode [];;

let innerPathLength startNode =
  let rec innerPathLengthAccum node level accum = match node with
  Empty -> accum
 | Node(v,l,r)->level+innerPathLengthAccum(l)(level+1)(innerPathLengthAccum(r)(level+1) accum)
in innerPathLengthAccum startNode 0 0;;

innerPathLength tt2;;

let outerPathLength startNode = 
  let rec outerPathLengthAccum node level accum = match node with
  Empty->accum+level
  |Node(v,l,r)->outerPathLengthAccum(l) (level+1) (outerPathLengthAccum r (level+1) accum)
in outerPathLengthAccum startNode 0 0;;

outerPathLength tt;;

(*Zadanie 5*)
let graphTest = Graph
(function
0 -> [3]
| 1 -> [0;2;4]
| 2 -> [1]
| 3 -> []
| 4 -> [0;2]
| n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
);;

let dephSearch (Graph g)(startNode) = 
  let rec dephSearchAccum visited stack = match stack with
   h::t when List.mem (h) visited -> dephSearchAccum visited t
  |h::t -> h::dephSearchAccum (h::visited)((g h)@t)
  |[] -> []
in dephSearchAccum [] [startNode];;


dephSearch graphTest 4 ;;

preOrder tt;;





