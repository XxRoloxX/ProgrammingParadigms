class Time(hourArg: Int):
  private var h:Int = if hourArg<0 then 0 else hourArg
  def hour: Int = h
  def hour_=(newVal: Int):Unit = h = if newVal<0 then 0 else newVal
  def setHour(newVal: Int):Unit = h = if newVal<5 then 10 else newVal

end Time

var g = new Time(10);
g.hour
g.hour = -10
g.hour

class TimeExt(hourArg:Int, minuteArg:Int):
  private var h:Int = {require(hourArg>=0 && hourArg<=24, s"hourArg=$hourArg"); hourArg}
  private var m:Int = {require(minuteArg>=0 && minuteArg<=60, s"minuteArg=$minuteArg"); minuteArg}

  def hour: Int = h
  def minute: Int = m;

  def hour_=(newVal: Int):Unit = {require(newVal>=0 && newVal<=24, s"newVal=$newVal"); h=newVal};
  def minute_=(newVal: Int):Unit = {require(newVal>=0 && newVal<=60, s"newVal=$newVal"); m=newVal};

  def before(other:TimeExt):Boolean =
    if h<other.hour then true
    else if h>other.hour then false
    else
      if m<other.minute then true
      else false






end TimeExt

var f = new TimeExt(2,10)
var f2 = new TimeExt(2,10)

f.before(f2)

class TimeExtOnlyMinutes(hourArg:Int, minuteArg:Int):

  private var m:Int = {require(minuteArg>=0 && minuteArg<=60 && hourArg>=0 && hourArg<=24, s"minuteArg:hourArg=$minuteArg:$hourArg");
    hourArg*60+minuteArg}

  def hour: Int = m / 60
  def minute: Int = m%60

  def hour_=(newVal: Int):Unit = {require(newVal>=0 && newVal<=24, s"newVal=$newVal"); m = newVal*60 + m%60};
  def minute_=(newVal: Int):Unit = {require(newVal>=0 && newVal<=60, s"newVal=$newVal"); m= (m-m%60) + newVal};

  def before(other:TimeExtOnlyMinutes):Boolean = m<other.m



end TimeExtOnlyMinutes

var f3 = new TimeExtOnlyMinutes(3,5);
f3.hour =24
f3.minute =6

f3.hour
f3.minute

class Vehicle(val producer:String, val model:String, val productionYear:Int=(-1), var registrationPlate:String=""):
  def this(producer:String,  model:String,  registrationPlate:String) =
    this(producer,model,-1,registrationPlate);
end Vehicle

var h = new Vehicle("Volvo", "Astra")
var h1 = new Vehicle("Volvo", "Astra", 1999)
var h2 = new Vehicle("Volvo", "Astra", 1999, "AB#$FD")
var h3 = new Vehicle("Volvo", "Astra", "AB#$FD")

class ExceptionTest:
  def metoda1() = metoda2();
  def metoda2() = metoda3();
  def metoda3() = throw new Exception("Wyjatek zgloszony w metoda3")

object MainTest:
  def main(args: Array[String]):Unit =
    var exceptionObject = new ExceptionTest;
    try
      exceptionObject.metoda1()
    catch
      case e:Exception =>
        println(e.getMessage)
        e.printStackTrace()


MainTest.main(Array(""));
/*
Wyjatek zgloszony w metoda3
  java.lang.Exception: Wyjatek zgloszony w metoda3
  at
rs$line$1$ExceptionTest.metoda3(rs$line$1: 4)
at rs$line$1$ExceptionTest
.metoda2(rs$line$1: 3)
at rs$line$1$ExceptionTest
.metoda1(rs$line$1: 2)
at rs$line$2$MainTest$
.main(rs$line$2: 5)

*/
/// Wywolanie wyjatku w metoda3 przekierowuje sterowanie do metody2, ktora tez zwraca wyjatek,
///następnie sterowanie zostaje przekierowanie do metody1, gdzie wyjatek powoduje przekierowania startowania
/// do main, gdzie wyjatek zostaje zlapany i wyswietlony, razem z droga ktora "przebył"




