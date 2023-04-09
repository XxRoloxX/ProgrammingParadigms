//import Maybe.Nothing
//import WeekDay.{DayIndex, DayName}

//Zadanie 1
type Point2D = (Double,Double)
type PointND = List[Double]


val point1:Point2D = (2.0,3.0)
val point2:Point2D = (3.0,4.0)

val distance = (point1:Point2D)=>(point2:Point2D) => Math.sqrt((point1(0)-point2(0))*(point1(0)-point2(0)) + (point1(1)-point2(1))*(point1(1)-point2(1)))

distance(point1)(point2)

//Zadanie 2

//Person type with tuples (anonymous)
type Person = (String,String,Int,Int)

val john:Person = ("John","Doe",20,3)
val jane:Person = ("Jane","Doe",21,4)


//Person type (class) with records - parameters are vars so they are modifiable
class RecordPerson(var name:String, var surname:String,var age:Int, var shoeSize:Int)

//Person type (class) with records - parameters are val so they are NOT modifiable
class UnmutableRecordPerson(val name:String, val surname:String,val age:Int, val shoeSize:Int)


val unmutableRecordPerson1 = UnmutableRecordPerson("John","Doe",20,3)

val recordJohn = RecordPerson(name ="John","Doe",30,3)

//No problems
//recordJohn.name = "New John"

//Returns error because of val
//unmutableRecordPerson1.name = "New John"

val recordJane = RecordPerson("Jane","Doe",21,4)


type Partnership = (Person,Person)

val partnership:Partnership = (john,jane)

val getYoungerPerson: (Partnership=>Person) =(partnership:Partnership)=> if partnership(0)(2)>partnership(1)(2) then partnership(1) else partnership(0)

val youngerPerson = getYoungerPerson(partnership)

class RecordPartnership (var person1: RecordPerson,var person2: RecordPerson)

val recordPartnershipJaneJohn = RecordPartnership(recordJohn,recordJane)

val getYoungerRecordPerson: (RecordPartnership=>RecordPerson) = (partnership:RecordPartnership)=>
  if partnership.person1.age<partnership.person2.age then partnership.person1 else partnership.person2


val recordYoungerPersonName = (getYoungerRecordPerson (recordPartnershipJaneJohn)).name

//Zadanie 3
/*
enum WeekDayOld(val index:Int):
  case Monday extends WeekDayOld(0)
  case Tuesday extends WeekDayOld(1)
  case Wednesday extends WeekDayOld(2)
  case Thursday extends WeekDayOld(3)
  case Friday extends WeekDayOld(4)
  case Saturday extends WeekDayOld(5)
  case Sunday extends WeekDayOld(6)
end WeekDayOld



val day = WeekDayOld.Monday

val ordinalValueOfDay = day.ordinal
*/

/*
val weekDayToString = (day:WeekDayOld) => day match
  case WeekDayOld.Monday  => "Monday"
  case WeekDayOld.Tuesday  => "Tuesday"
  case WeekDayOld.Wednesday  => "Wednesday"
  case WeekDayOld.Thursday  => "Thursday"
  case WeekDayOld.Friday  => "Friday"
  case WeekDayOld.Saturday => "Saturday"
  case WeekDayOld.Sunday  => "Sunday"
*/

enum WeekDay:
  case DayIndex(index:Int)
  case DayName(name:String)
end WeekDay



val dayIndexToString = (index:Int) => index match
  case 0 => "Monday"
  case 1 => "Tuesday"
  case 2 => "Wednesday"
  case 3 => "Thursday"
  case 4 => "Friday"
  case 5 => "Saturday"
  case 6 => "Sunday"
  case _ => "Incorrect index"

val dayNameToIndex = (name:String) => name match
  case "Monday" => 0
  case "Tuesday" => 1
  case "Wednesday" => 2
  case "Thursday" => 3
  case "Friday" => 4
  case "Saturday" => 5
  case "Sunday" => 6
  case _ => -1


val nextDay = (day:WeekDay) => day match
  case WeekDay.DayIndex(day) => WeekDay.DayIndex((day+1)%7)
  case WeekDay.DayName(day) => WeekDay.DayName(dayIndexToString((dayNameToIndex(day)+1)%7))

val weekDayToString = (day:WeekDay) => day match
  case WeekDay.DayIndex(day) => dayIndexToString(day)
  case WeekDay.DayName(day) => day



val testDay = WeekDay.DayName(weekDayToString(WeekDay.DayIndex(1)))
val nextTestDay = nextDay(testDay)

/*
enum Maybe[+A]:
  case Just(x:A) extends Maybe[A]
  case Nothing extextends Bt[Nothing]
nds Maybe[Nothing]
end Maybe
*/

//Zadanie 4

enum Maybe[+A]:
  case Just(x:A)
  case Nothing
end Maybe


def safeHead[A](list:List[A]):Maybe[A] = list match
  case List() => Maybe.Nothing
  case h::t => Maybe.Just(h)

val testHead = safeHead(List(1,2,3,4,5))
val testHead2 = safeHead(List(1))
val testHead3 = safeHead(List())
val testHead4 = safeHead(List("A"))

//Zadanie 5


class CuboidClass(var height:Double,var length:Double,var width:Double)
class SphereClass(var radius:Double)
class ConeClass(var height:Double,var radius:Double)
class CylinderClass(var height:Double,var radius:Double)



enum SolidFigure:
  case Cuboid(figure:CuboidClass)
  case Sphere(figure:SphereClass)
  case Cone(figure:ConeClass)
  case Cylinder(figure:CylinderClass)
end SolidFigure



val volume = (figure:SolidFigure) => figure match
  case SolidFigure.Cuboid(figure)=> figure.height*figure.width*figure.length
  case SolidFigure.Sphere(figure)=> (4.0/3.0)*Math.PI*figure.radius*figure.radius*figure.radius
  case SolidFigure.Cone(figure)=>(1.0/3.0)*Math.PI*figure.radius*figure.radius*figure.height
  case SolidFigure.Cylinder(figure)=> Math.PI*figure.radius*figure.radius*figure.height


val testFigure1 = CuboidClass(1.0,2.0,3.0)
val testFigure2 = SphereClass(2.0)


volume(SolidFigure.Cuboid(testFigure1))
volume(SolidFigure.Sphere(testFigure2))


