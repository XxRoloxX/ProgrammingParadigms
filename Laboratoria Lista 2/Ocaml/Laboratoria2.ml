
"Zadanie 1";;
1*9 +2 +3 ;;

2*1 -9 +10 ;;

5.0/.2.0 +. 9.4 +.1.0;;

"Zadanie 2";;

"Niemutowalna";;
let x = 99;;

x=100;;

"Mutowalna* - tak naprawdę mutowalna jest pamięć a nie sama zmienna stąd element ref";;
let y= ref 80;;

y:= 10;;

!y;;

"Zadanie 3";;

let sqr(x) = x*x ;;

sqr(10);;

sqr(20);;

let ratio((x,y)) = if y=0.0 then raise (Failure "dzielenie przez zero") else x/.y;;

ratio(2.0,3.0);;

ratio(1.0,2.0);;

ratio(3.0,0.0);;

let max((x,y)) = if x<y then y else x;;

max(10,2);;

max(5,1);;

"Zadanie 4";;
let is_even x = if x mod 2=0 then true else false;;

let is_bigger_than_ten x = if x>10 then true else false;;

let is_smaller_than_thousand x = if x<1000 then true else false;;

let rec test_function (f, x) = if f(x) = true then test_function(f,x+5) else test_function(f,x-5) ;;

test_function(is_even, 10);;

let rec sum_if_true (test, x, y, z) = if test(x)=false && x!=0 then sum_if_true(test, 0, y, z) else if 
  test(y)=false && y!=0 then sum_if_true(test, x, 0, z) else if test(z)=false && z!=0 then sum_if_true(test, x, y, 0)
  else x+y+z;;


  sum_if_true(is_even, 3, 1, 1);;

  sum_if_true(is_bigger_than_ten, 12,5, 17);;

  sum_if_true(is_smaller_than_thousand, 50, 2000, 6);;

"Zadanie 5";;


let friends = ["Paweł";"Nikodem";"Karolina";"Dominik";"Julia"];;
let new_friend = "Wojtek";;

let update_friend_list friends new_friend = [new_friend]@friends;;

update_friend_list friends new_friend;;

