

(*Zadanie 1*)


type point2D =  float*float;;
type pointND = float list;;

let distance (p1:point2D) (p2:point2D) = sqrt((fst p1 -. fst p2)*.(fst p1 -. fst p2) +.(snd p1 -. snd p2)*.(snd p1 -. snd p2));;

(*Zadanie 2*)

type person = string*string*int*int;;

(*Product types without records are immutable*)
type personRecord = 
{
  name: string;
  surname: string;
  age: int;
  shoeSize: int;
};;

(*Type with mutable parameters*)

type personRecordMutable = 
{
  name:  string ref;
  surname: string ref;
  age: int ref;
  shoeSize: int ref;
};;

(*Creating mutable fields with mutable key-word*)
type personRecordMutable2 = 
{
  mutable name:  string;
  mutable surname: string;
  mutable age: int;
  mutable shoeSize: int;
};;

let john: person = ("John", "Doe",2,3);;


type partnership = person*person;;

let partnerShipTest:partnership = (john,john);;

type partnershipRecord = {
  person1: personRecord;
  person2: personRecord;
};;


(*First assigment requires use of ref value, after you can simply use :=*)
let johnMutableRecord:personRecordMutable= {
  name = ref "John";
  surname = ref "Doe";
  age = ref 1;
  shoeSize=ref 2;
};;

(*When fields are created we use =, later for modification we use <-*)
let johnWithMutableKeyword:personRecordMutable2 = 
{
  name ="John";
  surname = "Doe";
  age = 1;
  shoeSize = 2;
};;

johnWithMutableKeyword.name <- "new new new mutable keywork john";;

let johnRecord: personRecord = {
  name = "John";
  surname = "Doe";
  age = 30;
  shoeSize=2;
};;

let janeRecord: personRecord = {
  name = "Jane";
  surname = "Doe";
  age = 20;
  shoeSize=5;
};;

let partnershipJaneJohn = {
  person1 = johnRecord;
  person2 = janeRecord;
};;

(*Modification of only one field - this actually returns new object with changed field*) 
let newJohn = {johnRecord with name = "newJohn"};;

let nonMutableJohn:person = ("Non mutable john","doe",30,20);;

let nonMutableJane:person = ("Non mutable jane","doe",20,15);;


(*Actuall modification of field using reference := when memory is already allocated*)

johnMutableRecord.name:="new John";;
johnMutableRecord.name;;
johnMutableRecord.name:="new new John";;

let nonRecordPartnerShip:partnership = (nonMutableJohn,nonMutableJane);;

let betterJohn: person = match john with 
(name, surname,age,shoeSize) ->  (name, surname,age+1,shoeSize)
|_-> john;;

let setName (person1:person) (newName:string) = match person1 with 
(name, surname,age,shoeSize) -> let newPerson: person =(newName, surname,age,shoeSize) in newPerson;;

let getYougerPerson (partnership1:partnershipRecord) = if partnership1.person1.age > partnership1.person2.age then partnership1.person2 else partnership1.person1;;

let getYougerPersonWithoutRecords (partnership1: partnership):person = match partnership1 with 
((name1,surname1,age1,size1), (name2,surname2,age2,size2)) -> if age1 < age2 then (name1,surname1,age1,size1) else (name2,surname2,age2,size2);;

let youngerPerson = getYougerPerson partnershipJaneJohn;;

let youngerPerson = getYougerPersonWithoutRecords nonRecordPartnerShip;;

(*Zadanie 3*)


let indexToName index = match index with
0->"Monday"
|1->"Tuesday"
|2->"Wednesday"
|3->"Thursday"
|4->"Friday"
|5->"Saturday"
|6->"Sunday"
|_->"Incorrect index";;

let nameToIndex name = match name with
"Monday" -> 0
|"Tuesday"->1
|"Wednesday"->2
|"Thursday"->3
|"Friday"->4
|"Saturday"->5
|"Sunday"->6
|_-> -1;;


type weekDay = DayIndex of int | DayName of string;;

let weekDayToString (day:weekDay) = match day with
DayIndex index -> indexToName index
|DayName name -> name;;

weekDayToString(DayName("Monday"));;

weekDayToString(DayIndex(3));;

let nextDay (day:weekDay) = match day with
DayIndex index ->  DayIndex ((index+1) mod 7)
|DayName name -> DayName (indexToName(((nameToIndex name)+1) mod 7));;


nextDay(DayIndex(1));;
nextDay(DayName("Monday"));;

(*Zadanie 4*)

type 'param maybe = Just of 'param | Nothing;;

let safeHead list = match list with
[] -> Nothing
|h::y -> Just(h);;

safeHead([1;2;3;4;6]);;
safeHead([1]);;
safeHead([]);;


(*Zadanie 5*)

type solidFigure =  
Cuboid of {height: float; length: float; width: float}
| Sphere of {radius: float} | Cone of {height: float; radius: float}
| Cylinder of {height: float; radius: float};;

let volume figure = match figure with  
Cuboid figure -> figure.height*.figure.length*.figure.width
|Sphere figure -> (4.0/.3.0)*.Stdlib.Float.pi*.figure.radius*.figure.radius*.figure.radius
|Cone figure -> (1.0/.3.0)*.Stdlib.Float.pi*.figure.radius*.figure.radius*.figure.height
|Cylinder figure -> Stdlib.Float.pi*.figure.radius*.figure.radius*.figure.height;;


let testFigure1 = Cuboid({height = 1.0; length = 2.0; width = 3.0});;
let testFigure2 = Sphere({radius = 2.0});;

let volume1 = volume(testFigure1);;

let volume2 = volume(testFigure2);;

