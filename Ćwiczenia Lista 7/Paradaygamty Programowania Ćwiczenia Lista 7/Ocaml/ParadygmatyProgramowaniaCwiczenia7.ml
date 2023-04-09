module type QUEUE_FUN =
sig
type 'a t
exception Empty of string
val empty: unit -> 'a t
val enqueue: 'a * 'a t -> 'a t
val dequeue: 'a t -> 'a t
val first: 'a t -> 'a
val isEmpty: 'a t -> bool
end;;

module Queue_list : QUEUE_FUN = struct
  
  type 'a t = 'a list 

  let empty () = []  

  exception Empty of string

  let enqueue (new_elem,queue) = queue@[new_elem]

  let dequeue (queue) = match queue with 
  h::t -> t
  |[]-> raise (Empty "empty queue")


  let first queue =  match queue with 
  h::t -> h
  |[]-> raise (Empty "empty queue")

  let isEmpty (queue) = match queue with 
  h::t-> false
  |[]->true



end;;

let simple_queue = Queue_list.empty();;

Queue_list.first simple_queue;;

Queue_list.dequeue simple_queue;;



let new_queue = Queue_list.enqueue ("a",simple_queue);;

let new_queue2 = Queue_list.enqueue ("b",new_queue);;

let new_queue3 = Queue_list.enqueue ("c",new_queue2);;

let firstElement1 = Queue_list.first new_queue3;;

let new_queue4 = Queue_list.dequeue (new_queue3);;

let firstElement2 = Queue_list.first new_queue4;;


module Queue_pair : QUEUE_FUN = struct
  
  type 'a t = ('a list) * ('a list)

  let empty () = ([],[])  

  exception Empty of string

  let normalize queue = match queue with
  ([],ending) -> (List.rev ending,[])
  |_ -> queue;;

  let enqueue (new_elem,queue) = match queue with 
  |(beginning,ending) -> normalize(beginning,new_elem::ending)


  let dequeue (queue) = match queue with 
  ([],_) -> raise (Empty "empty queue") 
  |(beg_head::beg_tail,ending) -> normalize(beg_tail,ending);;
  
  let first queue =  match queue with 
  |([],_)-> raise (Empty "empty queue")
  |(beg_head::beg_tail,_) -> beg_head 

  let isEmpty (queue) = match queue with 
  ([],_)-> true
  |_->false

end;;

let simple_queue = Queue_pair.empty();;

Queue_pair.first simple_queue;;

Queue_pair.dequeue simple_queue;;



let new_queue = Queue_pair.enqueue ("a",simple_queue);;

let new_queue2 = Queue_pair.enqueue ("b",new_queue);;

let new_queue3 = Queue_pair.enqueue ("c",new_queue2);;

let firstElement1 = Queue_pair.first new_queue3;;

let new_queue4 = Queue_pair.dequeue (new_queue3);;

let firstElement2 = Queue_pair.first new_queue4;;

let new_queue4 = Queue_pair.dequeue (new_queue4);;

let firstElement2 = Queue_pair.first new_queue4;;

(*Zadanie 2*)
module type QUEUE_MUT =
sig
type 'a element_type = EmptyElement | Element of 'a | Cyclic of 'a element_type array 

type 'a t
(* The type of queues containing elements of type ['a]. *)
exception Empty of string
(* Raised when [first q] is applied to an empty queue [q]. *)
exception Full of string
(* Raised when [enqueue(x,q)] is applied to a full queue [q]. *)
val empty: int -> 'a t
(* [empty n] returns a new queue of length [n], initially empty. *)
val enqueue: 'a * 'a t -> unit
(* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *)
val dequeue: 'a t -> unit
(* [dequeue q] removes the first element in queue [q] *)
val first: 'a t -> 'a
(* [first q] returns the first element in queue [q] without removing
it from the queue, or raises [Empty] if the queue is empty. *)
val isEmpty: 'a t -> bool
(* [isEmpty q] returns [true] if queue [q] is empty,
otherwise returns [false]. *)
val isFull: 'a t -> bool

val return_cycle: 'a t -> 'a element_type
(* [isFull q] returns [true] if queue [q] is full,
otherwise returns [false]. *)
end;;

module Queue_cyclic:QUEUE_MUT = struct 
  type 'a element_type = EmptyElement | Element of 'a | Cyclic of 'a element_type array 
  type 'a t = {arr: 'a element_type array; mutable actual_length: int}

  exception Empty of string
  
  exception Full of string

  let empty (initial_size) = {arr =Array.init (initial_size+1) (fun n -> EmptyElement);actual_length=0}

  let isFull queue = if queue.actual_length<((Array.length queue.arr) -1) then false else true

  let isEmpty queue = if queue.actual_length=0 then true else false

  let enqueue (new_element,queue) = match isFull queue with
  false-> queue.arr.(queue.actual_length)<-Element(new_element);queue.arr.(queue.actual_length+1)<-Cyclic(queue.arr) ;queue.actual_length <- queue.actual_length +1
  |true->raise (Full "queue is full")

  let dequeue (queue) = match isEmpty queue with
  false->  queue.actual_length<-queue.actual_length-1;
    let rec dequeue_recursive queue actual_index = if actual_index <(queue.actual_length+1)  then (queue.arr.(actual_index)<-queue.arr.(actual_index+1); dequeue_recursive queue (actual_index+1)) in dequeue_recursive queue 0
      ; queue.arr.(queue.actual_length+1)<-EmptyElement;
  |true->raise (Empty "queue is empty")

  let first (queue) = match isEmpty queue with
  true -> raise (Empty "queue is empty")
  |false-> let Element(result) =  queue.arr.(0) in result

  let return_cycle queue = queue.arr.(queue.actual_length)
  
end;;

(*
module Queue_cyclic:QUEUE_MUT = struct
  type 'a t = {mutable l: 'a list;maximum_length: int; mutable actual_length: int}
  exception Empty of string
  exception Full of string
  let empty initial_length = {l=[];maximum_length=initial_length; actual_length=0}

  let isFull queue = queue.actual_length=queue.maximum_length

  let isEmpty queue = queue.actual_length=0

  let create_list original_cyclic_list new_element list_size = let rec create_list_inner cyclic_list list_size = match (cyclic_list,list_size) with
  (_,_) when list_size <1 -> new_element::original_cyclic_list
  |(h::t,_) -> h::create_list_inner t (list_size-1) 
in create_list_inner original_cyclic_list list_size;;

let dequeue_list_creation original_cyclic list_size = let rec dequeue_list_creation_inner cyclic_list list_size = match (cyclic_list,list_size) with
(_,_) when list_size <1 -> dequeue_list_creation_inner cyclic_list list_size
|(h::t,_) -> h::dequeue_list_creation_inner t (list_size-1) 
in dequeue_list_creation_inner original_cyclic list_size;;

  let enqueue (new_elem,queue) = match (isFull queue) with
    false ->queue.l <- create_list (queue.l) (new_elem) (queue.actual_length); queue.actual_length<-queue.actual_length+1
    |true-> raise (Full "full queue")

  let dequeue (queue) = match isEmpty queue with 
    true -> raise (Full "empty queue")
    |false when queue.actual_length=1 ->queue.l <- []; queue.actual_length<-0
    |false -> (match queue.l with h::t -> queue.l <- dequeue_list_creation queue.l queue.actual_length; queue.actual_length<-queue.actual_length-1)

  let first queue = match isEmpty queue with
  true-> raise (Full "empty queue")
  |false -> let h::t = queue.l in h

end;;
*)
let my_queue = Queue_cyclic.empty 3;;

Queue_cyclic.enqueue (1,my_queue);;
Queue_cyclic.enqueue (2,my_queue);;
Queue_cyclic.enqueue (3,my_queue);;
Queue_cyclic.enqueue (4,my_queue);;
Queue_cyclic.first (my_queue);;
Queue_cyclic.first (my_queue);;
Queue_cyclic.dequeue (my_queue);;
Queue_cyclic.first (my_queue);;
Queue_cyclic.dequeue (my_queue);;
Queue_cyclic.first (my_queue);;
Queue_cyclic.dequeue (my_queue);;

Queue_cyclic.first (my_queue);;
Queue_cyclic.dequeue (my_queue);;
type 'a element_type = EmptyElement | Element of 'a | Cyclic of 'a element_type array ;;
let test_cycle = Queue_cyclic.return_cycle my_queue;;

let test = match test_cycle with 
Cyclic(result)-> result.(2);;
;;





