let rec get_length list = if list = [] then 0 
else 1+ get_length(List.tl list);;

let rec get_last_element list = if get_length(list) = 0 then None 
else if get_length(list)=1 then Some(List.hd list)
else get_last_element(List.tl list);;


get_last_element [1;2;3;4;5;6;7;8;9];;

let rec get_last_two_elements list = if get_length(list) = 0 then None 
else if get_length(list)=2 then Some list 
else get_last_two_elements(List.tl list);;

let rec reverse_order list = if get_length(list) = 0 then list
 else reverse_order(List.tl list)@[List.hd list];;

reverse_order [1;2;3;4;5;6;7;8];;

let get_value_from_some x = match x with 
None->0 | 
Some(x)->x;;


let rec is_palindrome list = if get_length list <=1 then true 
else if (List.hd list = get_value_from_some(get_last_element(list))) then is_palindrome(List.tl (reverse_order(List.tl list))) 
else false;;

is_palindrome [1;2;3;3;2;1];;


let rec is_element_in_table (list,element) = if list=[] then false 
else if List.hd list = element then true
 else is_element_in_table((List.tl list), element);;

let rec remove_repetative_elements list = if list=[] then []
 else if is_element_in_table((List.tl list), List.hd list) then remove_repetative_elements(List.tl list) 
else [List.hd list]@remove_repetative_elements(List.tl list);;

remove_repetative_elements [3;3;3;4;2;4];;

let rec get_elements_from_even_indexes list = if list=[] then list
 else if ((get_length list)-1) mod 2 =0 then get_elements_from_even_indexes(reverse_order(List.tl (reverse_order list)))@[get_value_from_some(get_last_element(list))]
 else get_elements_from_even_indexes(reverse_order(List.tl (reverse_order list)));;

 get_elements_from_even_indexes [0;1;2;3;4;5;6;7;8;9;10];;

let rec get_list_of_dividers (number, accumulator) = if accumulator*accumulator >= number || accumulator<2 then [] 
else if number mod accumulator = 0 then get_list_of_dividers(number, accumulator+1)@[accumulator]
else get_list_of_dividers (number, accumulator+1);;

let rec is_prime_with_accumulator number accumulator=  if accumulator*accumulator > number then true
else if number mod accumulator = 0 then false
else is_prime_with_accumulator number (accumulator+1) ;;

let is_prime number = if number< 2 then false 
else if number<4 then true 
else is_prime_with_accumulator number 2;;

 is_prime 69;;

