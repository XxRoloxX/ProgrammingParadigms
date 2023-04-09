(*Zadanie 1*)
let f1 x = x 2 2;;

let f2 x y z = x (y^z);;

(*Zadanie 2*)

let curry3Sugar f x y z = f(x,y,z);;

let curry3 = fun f ->fun x->fun y-> fun z -> f(x,y,z);;


let uncurry3Sugar (f)(x,y,z)= f x y z;;

let uncurry = fun f -> fun (x,y,z)-> f x y z;;


(*Zadanie 3*)
let sumProdHelp el1 el2 = match el1 with
(h,t)->(h+el2,t*el2);;

let sumProd xs = List.fold_left (fun el1 el2 -> match el1 with(h,t)->(h+el2,t*el2)) (0,1) xs;;

sumProd [1;2;3];;

(*Zadanie 4*)

(*Nieskończona rekursja: Problem polega na tym,że let large - posiada elmenty większe lub RÓWNE głowie listy, przez co jak mamy liste która jest posortowana
    (lub posortowany fragment listy, to za każdym razem przekazywana jest ta sama lista (bo wszystkie elementy są większe lub równe głowie))*)

let rec quicksort = function
  []->[]
  |[x]->[x]
  |xs-> let small = List.filter (fun y->y <List.hd xs) xs
  and large = List.filter (fun y->y >=List.hd xs) xs
in quicksort small @ quicksort large;;


(*Probem polega na tym, że w liście może być więcej niż jeden element o tej samej wartości co głowa, a do ostatecznej listy będzie dodana tylko jedna wartość równa głowie*)
let rec quicksort' =  function
  []->[]
  |x::xs-> let small = List.filter (fun y->y <x) xs
  and large = List.filter (fun y->y >x) xs
in quicksort' small @ (x::quicksort' large);; (*Dodanie tylko raz (a mogło być 3 wartości równie głowie, które się nie pojawią w small albo large)*)

quicksort'([2;3;1;2]);;


let is_bigger el1 el2 = el1>el2;;

(*Zadanie 5*)

let rec put_element_in_correct_spot list element pred accum = match list with
    []->(List.rev accum)@[element]
    |h::t when pred h element -> (List.rev accum)@[element]@list
    |h::t -> put_element_in_correct_spot (List.tl list) element pred (h::accum);;

put_element_in_correct_spot [1;2;3;4;5] (3) is_bigger [];;

let insertSort pred list = let rec insertSortaccum list accum = match list with
         []->accum
         |h::t -> insertSortaccum t (put_element_in_correct_spot accum h pred [])
      in insertSortaccum list [];;

    insertSort is_bigger [5;4;3;5;2];;


let merge list1 list2 pred = let rec mergeAccum list1 list2 accum = match (list1,list2) with
(h1::t1,h2::t2) when pred h1 h2 -> mergeAccum list1 t2 (h2::accum)
|(h1::t1,h2::t2) -> mergeAccum t1 list2 (h1::accum)
|([], h2::t2) -> (List.rev accum)@(list2)
|(h1::t1,[])->(List.rev accum)@(list1)
|(_,_)->(List.rev accum)
    in mergeAccum list1 list2 [];;

merge [1;2;3;4;5;6] [7] is_bigger;;

let split_list_in_half list = let rec split_list_in_half_accum list accum n = match list with
[]-> ([],[])
|h::t when n=0 -> let (p,s)=accum in (List.rev p, list)
|h::t -> let (p,s)=accum in split_list_in_half_accum t (h::p,s) (n-1)

  in split_list_in_half_accum list ([],[]) ((List.length list)/2);;

  split_list_in_half [1;2];;

let is_more_than_one_element list = let h::t = list in (if t=[] then false else true);;


let mergeSort list pred = let rec mergeSortAccum list pred= match list with
|h::t when (is_more_than_one_element list) -> let (left,right) = split_list_in_half list in merge (mergeSortAccum left pred) (mergeSortAccum right pred) pred
|h::t-> h::t
|[]->[]
in mergeSortAccum list pred ;;

mergeSort [5;2;3;5;7;4;2;1] is_bigger;;

mergeSort [2;3;4;5;6;7;8;9;-2] is_bigger;;



