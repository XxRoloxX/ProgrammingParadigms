let rec flatten xss = if xss =[] then []
 else List.hd (xss) @ flatten (List.tl xss) ;;
let xs = flatten([[1;2;3];[4;5;6];[22;33;44];[]]);;


let rec count (x,xs) = if xs = [] then 0 
  else if (List.hd xs) = x then count (x,List.tl xs)+1 
  else count (x,List.tl xs);;

let xs = count((4,[4;4;3;1;7;3;11;3;4]));;


let rec replicate (x,n) = if n=0 then [] 
  else if n>0 then [x]@replicate(x,n-1)
    else raise (Failure "ujemny argument");;
  
let xs = replicate("a", -1);;

let rec sqrList (xs) = if xs=[] then []
else [(List.hd xs) * (List.hd xs)]@sqrList(List.tl xs);;

let xs = sqrList([1;4;2;3;6]);;


let rec palindrome (xs) = if (List.length xs) <=1 then true
else if List.hd xs != List.hd (List.rev xs) then false
else palindrome(List.tl (List.rev (List.tl xs)));;

let rec palindrome2 (xs) = List.rev xs = xs;;

let xs = palindrome([2;2;2]);;

let rec listLength (xs) = if xs = [] then 0
else listLength(List.tl xs)+1;;

let xs = listLength([4;4;4;4;1]);;
