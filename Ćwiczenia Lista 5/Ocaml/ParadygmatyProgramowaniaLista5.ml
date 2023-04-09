(*Zadanie 1*)
type 'a llist = LNil | LCons of 'a*'a llist Lazy.t;;

let rec natural n= LCons(n,lazy(natural (n+1)));;

let lrepeat list n = let rec lrepeat_inner list left = match (list,left) with
(LNil,_)->LNil
|(LCons(h,lazy t),left) when left>1 -> LCons(h,lazy(lrepeat_inner(list)(left-1)))
|(LCons(h,lazy t),left) when left=1 -> LCons(h,lazy(lrepeat_inner(t)(n)))
| _ when left<1 -> LNil
in lrepeat_inner list n;;

(*
let lrepeat list n = 
  let w = n in
  let rec lrepeat_inner list left = 
    match (list,left) with
      (LNil,_)->LNil
      | (LCons(x, xf), left) -> if left > 1 then LCons(x, lrepeat_inner(list, left - 1))
      else lrepeat_inner xf() w
    lrepeat_inner list n;;
    *)


let naturals_repeatet = lrepeat (natural 0) 2;;

let rec print_n_elements list n = match (list,n) with
(LNil,_) -> []
|(_,0)->[]
|(LCons(h,lazy t),n)-> h::print_n_elements(t)(n-1);;

print_n_elements (lrepeat (natural 0) 2) 11;;

(*Zadanie 2*)

let rec fibTail n = let rec fibIter(n,iter,accum1, accum2) = if n==0 then 0 
else if n==iter then accum1 
else fibIter(n,iter+1, accum2, accum1+accum2) in fibIter(n,0,0,1);;

let lfib = let rec lfibinner prev cur = LCons(prev, lazy(lfibinner cur (prev + cur))) in lfibinner 0 1;;


let lfib = let rec lfib_prep n = LCons(fibTail(n), lazy(lfib_prep(n+1))) in lfib_prep (0);;


print_n_elements (lfib) 10;;

(*Zadanie 3*)

type 'a lBT = LEmpty | LNode of 'a*(unit->'a lBT)*(unit->'a lBT);;

let rec toLazyList = function
[] -> LNil
| x :: xs -> LCons(x, lazy (toLazyList xs))
(*
let rec iBreadth ltree = let rec iBreadthHelp queue result = match queue with
[]-> result
|LNode(value,left,right)::t -> iBreadthHelp (t@([left()]@[right()]))(value::result)
|LEmpty::t -> iBreadthHelp (t)(result) 
in toLazyList (iBreadthHelp ltree []);;
*)


let rec iBreadth2 ltree = let rec iBreadthHelp2 queue = match queue with
[]-> LNil
|LNode(value,left,right)::t -> LCons(value,lazy (iBreadthHelp2(t@([left()]@[right()]))))
in iBreadthHelp2 [ltree];;

let rec lTree n = LNode(n, (fun ()->lTree(2*n)), (fun ()->lTree(2*n+1)));;

let result_tree = print_n_elements (iBreadth2(lTree 0)) 20;;

