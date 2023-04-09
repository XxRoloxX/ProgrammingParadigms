
(*Zadanie 1*)
let memoized_stirling_hash = Hashtbl.create 10;;

(*
let testTableFunction hash_table = Hashtbl.add hash_table 3 3;;

testTableFunction memoized_stirling_hash;;

Hashtbl.add memoized_stirling_hash 4 2;;

Hashtbl.length memoized_stirling_hash;;

*)
(*
let memoized_stirling_hash = ref (Hashtbl.create 10);;


let testTableFunction ref hash_table = Hashtbl.add hash_table 3;;

testTableFunction memoized_stirling_hash;;

Hashtbl.add !(memoized_stirling_hash) 4 2;;

Hashtbl.length !(memoized_stirling_hash);;
*)



let rec stirling n m = match (n,m) with 
(_,1) -> 1
|(h1,h2) when h1=h2 ->1
|_ -> stirling(n-1)(m-1) + m*stirling(n-1)(m)
;;


(*
stirling 100 50;;*)
let rec memoized_stirling hash_table n m = match (n,m) with 
(_,1) -> 1
|(h1,h2) when h1=h2 ->1
| _  -> match (Hashtbl.find_opt hash_table (n,m)) with 
   None -> let result =  (memoized_stirling(hash_table)(n-1)(m-1) + m*memoized_stirling(hash_table)(n-1)(m)) in Hashtbl.add hash_table (n,m) (result); result
  |Some(result) -> result;; 

memoized_stirling memoized_stirling_hash 100 50;;

let rec memoized_stirling hash_table n m = match (n,m) with 
(_,1) -> 1
|(h1,h2) when h1=h2 ->1
| _  -> match (Hashtbl.find_opt hash_table (n,m)) with 
   None -> let result =  (memoized_stirling(hash_table)(n-1)(m-1) + m*memoized_stirling(hash_table)(n-1)(m)) in Hashtbl.add hash_table (n,m) (result); result
  |Some(result) -> result;; 

(*
  let rec memoized_stirling hash_table n m = match (n,m) with 
  (_,1) -> 1
  |(h1,h2) when h1=h2 ->1
  | _  -> match (Hashtbl.find_opt hash_table (n,m)) with 
     None -> let result =  (memoized_stirling(hash_table)(n-1)(m-1) + m*memoized_stirling(hash_table)(n-1)(m)) in Hashtbl.add hash_table (n,m) (result); result
    |Some(result) -> result;; 

memoized_stirling memoized_stirling_hash  100 50;;
*)


(*Zadanie 2*)
let make_memoize_hash_table = Hashtbl.create 10;;
let add_numbers (x,y) = x+y;;

let rec fib n = if n=0 then 0 else if n=1 then 1 else fib (n-1) + fib (n-2);;

let make_memoize hash_table f arg =  match Hashtbl.find_opt hash_table arg with
None -> let result = (f arg) in Hashtbl.add hash_table arg result; result
|Some(result)->result;;

(*
let make_memoize f arg = let hash_table = (Hashtbl.create 10) in match Hashtbl.find_opt hash_table arg with
None -> let result = (f arg) in in make_better_memoize_inner

*)

(*
let make_memoize hash_table f arg =  match Hashtbl.find_opt hash_table arg with
None -> let result = (make_memoize hash_table f arg) in Hashtbl.add hash_table arg result; result
|Some(result)->result;;

*)



let memoize_fib = make_memoize make_memoize_hash_table fib;;

fib 40;;
memoize_fib 46;;
(*fib 41;;*)


(*Zadanie 3*)

let lazy_x = lazy (stirling 30 10);; 
Lazy.force lazy_x;;

(*Zadanie 4*)

(*a*)
type 'a stream = Nil | Cons of 'a* 'a stream;;

let bell_number n = let rec bell_number_accum n k accum = 
  if k<=n then bell_number_accum(n)(k+1)(accum + memoized_stirling memoized_stirling_hash n k)
  else accum
in bell_number_accum n 1 0;;

type bell = Nil | Cons of int*(unit->bell);;

type 'a stream = Nil | Cons of 'a*(unit->'a stream);;

let rec bell_generation n = Cons(bell_number n , fun ()-> bell_generation(n+1));;

bell_number 5;;

memoized_stirling memoized_stirling_hash 5 2;;
stirling 30 1;;

bell_generation 10;;

(*b*)
let stream_head stream_generator n = match stream_generator n with
Cons(head,thunk)-> head;;


stream_head (bell_generation) 10;;

let stream_tail n = match bell_generation n with
Cons(head,thunk)-> thunk();;

(stream_tail 10);;

(*c*)
let n_elements_of_stream (stream_generator) n = let rec n_elements_of_stream_accum stream_generator n accum = match (stream_generator,n) with
(_, 0) -> List.rev accum
|(Cons(head,thunk), n) -> n_elements_of_stream_accum(thunk())(n-1)(head::accum)
|(Nil,_) ->List.rev accum 
in n_elements_of_stream_accum stream_generator n [];;

n_elements_of_stream (bell_generation 1) 10;;

let n_elements_of_stream_even (stream_generator) n = let rec n_elements_of_stream_even_accum stream_generator n skip accum = match (stream_generator,n,skip) with
(_, 0,_) -> List.rev accum
|(Cons(head,thunk), n,true) -> n_elements_of_stream_even_accum(thunk())(n-1)(false)(head::accum)
|(Cons(head,thunk), n,false) -> n_elements_of_stream_even_accum(thunk())(n)(true)(accum)
|(Nil,__,_) ->List.rev accum 
in n_elements_of_stream_even_accum stream_generator n true [];;

n_elements_of_stream_even (bell_generation 1) 10;;


let rec skip_n_elements stream_generator n = match (stream_generator,n) with 
(Nil,_) -> Nil
|(_,0) -> stream_generator
|(Cons(head,thunk),n)-> skip_n_elements(thunk())(n-1);;

skip_n_elements (bell_generation 1) 3;;

let rec from n=Cons(n,fun()->from (n + 1));;
let natural=from 0;;
let natural1=from 1;;

let bell = bell_generation 1;;

let rec combinedStreams s1 s2 n = match (s1,s2, n) with
(_,_,0)-> Nil 
|(Cons(h1,t1),Cons(h2,t2),n) -> Cons((h1,h2), fun () -> combinedStreams (t1()) (t2()) (n-1)) 
|(_,_,_) -> Nil;;

let naturals = combinedStreams natural natural1 4;;

let print_combined_streams s1 s2 n = n_elements_of_stream (combinedStreams s1 s2 n) n;;

print_combined_streams natural natural1 5;;

let square x = x*x;;

let rec map_stream f stream_generator = match (stream_generator) with 
Cons(h,t)-> Cons(f h,fun () -> map_stream (f) (t()))
|Nil -> Nil;;

let squared_natural = map_stream square natural;;

n_elements_of_stream squared_natural 5;;

