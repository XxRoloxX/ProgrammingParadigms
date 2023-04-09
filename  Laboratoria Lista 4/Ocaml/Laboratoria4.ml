(*UI*)
let reverse_order list = let rec reverse_order_accum(list, accum) = match list with
[]-> accum
| h::t-> reverse_order_accum(t,h::accum)
in reverse_order_accum(list,[]);;

let read_int_table size = let rec read_int_table_accum size accum = if size>0 then let new_value = read_int() in read_int_table_accum (size-1)(new_value::accum)
  else accum
in reverse_order (read_int_table_accum size []);;



(*Zadanie 0*)
let uppercase_ascii text = let rec uppercase_ascii_accum text i accum = if i<0 then accum 
else if Char.code(text.[i])>=97 && Char.code(text.[i])<=122 then uppercase_ascii_accum(text)(i-1)((String.make 1 (Char.chr(Char.code(text.[i])-32)))^accum)
else uppercase_ascii_accum(text)(i-1)(accum)
in uppercase_ascii_accum(text)((String.length text)-1)("");;



let choose_color text = let text_uppercase = (uppercase_ascii text) in match text_uppercase with
"WARN"->1
|"DEBUG"->2
|"INFO"->3
|"CRITICAL"->4
|_->0;;



let log prefix date text =  Printf.printf "\027[3%dm[%s] %s  %s \027[30m\n" (choose_color prefix) (uppercase_ascii prefix) date text;;
(*To color output you have to use escape sequance - witch decimal code is \027 (format is \ddd) and then linux code for setting color ex [31m is red, [32m is green ect.*)

let nightLog = log "debug";;

let date = nightLog "2022-10-26 01:45";;

let text = date "Hello";;


let plus x y = x+y;;

let rec get_length list = if list = [] then 0 
else 1+ get_length(List.tl list);;

let reverse_order list = let rec reverse_order_accum(list, accum) = match list with
[]-> accum
| h::t-> reverse_order_accum(t,h::accum)
in reverse_order_accum(list,[]);;

reverse_order [1;2;3;4;5;6;7;8];;

(*Zadanie 1*)
let map list f = 
  let rec map_accumulate list f accum = match list with
  []->accum
  | h::t -> (map_accumulate [@tailcall]) t f ((f h)::accum)
 in reverse_order(map_accumulate list f []);;




(*
 let map list f = 
  let rec map_accumulate list f accum = match list with
  []->accum
  | h::t -> (map_accumulate [@tailcall]) (List.tl list) f (accum@[(f h)])
 in map_accumulate list f [];;
*)
  (*
  [] -> []
  | h::t -> f h::map (List.tl list) f ;;*)


map [1;2;3;4;5;6] (fun x->x*x);;

(*
let filter list pred = 
  let rec filter_accum list pred accum=  match list with 
  []->accum
  | h::t -> if pred h then filter_accum t pred (h::accum)
  else (filter_accum [@tailcall]) t pred accum
in reverse_order(filter_accum list pred []);;
*)

(*Zadanie 2*)

let filter list pred = 
  let rec filter_accum list pred accum=  match list with 
  []->accum
  | h::t when pred h -> filter_accum t pred (h::accum)
  | h::t -> (filter_accum [@tailcall]) t pred accum
in reverse_order(filter_accum list pred []);;
 
filter [1;2;3;4;5;6;7;8;9] (fun x -> x mod 2 =1);;

(*Zadanie 3*)
let rec reduce list op accum = match list with 
  []->accum
  |h::t-> (reduce[@tailcall]) t op (op accum h);;

reduce [1;2;3;4;5;6;7;8;9] (fun x y -> (x+y)) 0 ;;
(*Zadanie 4*)
let average list = match list with
[]-> 0.0
|h::t ->float(reduce list (+) 0)/.float(get_length list);;

average [5;4;5;4];;


(*Zadanie 5*)
let is_upper_case letter = Char.code letter  >=65 && Char.code letter <=90;;
let concenate_first_letter first_text second_text = if String.length second_text >0 then first_text^(String.make 1 second_text.[0]) else first_text;;
(*
let string_to_list text = let rec string_to_list_accum i accum:(char list) = if i<0 then accum
else string_to_list_accum (i-1) ((text.[i])::accum) 
in string_to_list_accum (String.length text -1)([]);;

let char_list_to_string list = let rec char_list_to_string_accum list accum = match list with 
[]->accum
|(h:char)::t -> char_list_to_string_accum (List.tl list) h^accum in char_list_to_string_accum list "";;
*)

let string_to_list_of_chars text = let rec string_to_list_of_chars_accum text accum n 
= if n<0 then accum else string_to_list_of_chars_accum text (text.[n]::accum) (n-1)
in string_to_list_of_chars_accum text [] (String.length text-1);;

string_to_list_of_chars "ABCDEFGH";;
string_to_list_of_chars "Zakład Ubezpieczeń Społecznych";;

let list_of_chars_to_string list = let rec list_of_chars_to_string_accum list accum =
  match list with
  []->accum
  |h::t->list_of_chars_to_string_accum t (accum^(String.make 1 h))
in list_of_chars_to_string_accum list "";;

let acronym_help accum char2 = match (accum,char2) with 
  ([],_)->[char2]
| (h::t,ch) when (h=' ' || ch=' ') -> ch::accum
| (_,_)-> accum ;;

acronym_help [' ';'A';'B';] 'U';;

let acronym text = reduce (String.split_on_char  ' ' text) concenate_first_letter "";;

acronym " Zakład Ubezpieczeń Społecznych U";;

let acronym2 text = list_of_chars_to_string( reverse_order(filter (reduce (string_to_list_of_chars text) acronym_help [])(fun x->x!=' ') ));;

acronym2 "Zakład Ubezpieczeń Społecznych U";;
acronym2 "Jan Paweł 2";;
acronym2 "Zakład";;
acronym2 "Z";;
acronym2 "";;
acronym2 "  ZUS  ";;



(*Zadanie 6*)
let sum list= reduce list (+) 0;;

let sum_of_squares list = let sum_of_list = sum list 
in map (filter list (fun x-> x*x*x<=(sum_of_list))) (fun x->x*x);;

sum [1;2;3;4];;

sum_of_squares [];;

(*Printf.printf "\027[32m RED \027[0m NORMAL \n";;*)

(*UI*)

average (read_int_table 10);;

acronym (read_line());;


(*map (read_int_table 2)(fun x->x*x);;*)


let options_text = "1. Map\n2. Filter\n3.Acronym\n4.Sum of squares witch cubes are smaller than sum of all elements\nChoice: ";;
(*
let run_ui = let show_options =  let () = print_string options_text 
in let i=read_int () in if i>=1 && i<=3 then print_string 
in show_options;; *)
