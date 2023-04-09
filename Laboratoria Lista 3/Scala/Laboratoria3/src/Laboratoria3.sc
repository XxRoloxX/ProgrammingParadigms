def get_length[A](list:List[A]):Int = if list == List() then 0
else 1+get_length(list.tail)



def get_last_element[A](list:List[A]):Option[A] = if get_length(list)==0 then None
else if get_length(list)==1 then Some(list.head) else get_last_element(list.tail)


get_last_element(List(1,2,3,4,5,6,7,3))


def get_two_last_elements[A](list:List[A]):Option[List[A]] = if get_length(list)<=1 then None
else if get_length(list)==2 then Some(list)
else get_two_last_elements(list.tail)

get_two_last_elements(List(1,2))

def reverse_order[A](list:List[A]):List[A] = if list == List() then List()
else reverse_order(list.tail):::List(list.head)

reverse_order(List(1,2,3,4,5,6,7,8,9))

//To compare elements that are Options (Some(value)) you have to use .contains() !!!

def palindrome[A](list:List[A]):Boolean = if get_length(list)<=1 then true
else if !get_last_element(list).contains(list.head) then false
else palindrome(reverse_order(list.tail).tail)

palindrome(List(1,2,2,1))

def is_element_in_table[A](list:List[A], element:A):Boolean = if list == List() then false
else if list.head == element then true
else is_element_in_table(list.tail, element)

def remove_repetitive[A](list:List[A]):List[A] = if list==List() then List()
else if is_element_in_table(list.tail, list.head) then remove_repetitive(list.tail)
else List(list.head):::remove_repetitive(list.tail)


remove_repetitive(List(1,2,3,4,32,2,4,3,1,6,7,5,1,2))

//To convert Some(x) into x just do x.get
def return_element_with_even_indexes[A](list:List[A]):List[A] = if list==List() then List()
else if (get_length(list)-1)% 2 ==0 then return_element_with_even_indexes(reverse_order(reverse_order(list).tail)):::List(get_last_element(list).get)
else return_element_with_even_indexes(reverse_order(reverse_order(list).tail))

return_element_with_even_indexes(List(1,2,3,4,5,6,7,8,9,10,11,12,13))


def is_prime_with_accumulator(number:Int, accumulator:Int):Boolean = if accumulator*accumulator>number then true
else if number%accumulator == 0 then false
else is_prime_with_accumulator(number, accumulator+1)

def is_prime(number:Int):Boolean = if number<=1 then false
else if number<4 then true
else is_prime_with_accumulator(number, 2)

is_prime(4)