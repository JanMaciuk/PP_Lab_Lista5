import scala.language.postfixOps

@main
def main(): Unit = {
  println("\nZadanie 1:")
  println(distance((1, 2), (3, 4)))
  println("\nZadanie 2:")
  println(getYoungerV1(("Jan", "Maciuk", 20, true, 42), ("Anna", "Szybka", 19, false, 38)))
  println("\nZadanie 3:")
  println(weekDayToString(WeekDay.Monday))
  println(weekDayToString(nextDay(WeekDay.Monday)))
  println("\nZadanie 4:")
  println(safeHead(List(5,4,3,2,1)))
  println(safeHead(List()))
  println("\nZadanie 5:")
  println(Volume(Figure.Cuboid(2,2,2)))
  println(Volume(Figure.Sphere(5)))
}


//Zadanie 1:
type Point2D = (Float, Float)
private def distance(p1: Point2D, p2: Point2D): Float = {
  val (x1, y1) = p1 // Assign variables to values within Point2D touple to operate on them
  val (x2, y2) = p2
  Math.sqrt(Math.pow(x1 - x2, 2) + Math.pow(y1 - y2, 2)).toFloat
}


//Zadanie 2:
type PersonV1 = (String,String,Int,Boolean,Int) // (name, surname, age, isMale, shoeSize)
type PartnershipV1 = (PersonV1,PersonV1)
private def getYoungerV1(partnership: PartnershipV1): PersonV1 = {
  val (p1, p2) = partnership
  if (p1._3 < p2._3) p1 else p2
}
private class PersonClass(name:String, surname:String, age:Int, isMale:Boolean, shoeSize:Int) {
  var Name: String = name
  var Surname: String = surname
  var Age: Int = age
  var IsMale: Boolean = isMale
  var ShoeSize: Int = shoeSize
}
type PersonV2 = PersonClass
type PartnershipV2 = (PersonV2,PersonV2)
private def getYoungerV2(partnership: PartnershipV2): PersonV2 = {
  val (p1, p2) = partnership
  if (p1.Age < p2.Age) p1 else p2
}


  //Zadanie 3:
  private def weekDayToString(day: WeekDay): String = {
    day match {
      case WeekDay.Monday => "Poniedziałek"
      case WeekDay.Tuesday => "Wtorek"
      case WeekDay.Wednesday => "Środa"
      case WeekDay.Thursday => "Czwartek"
      case WeekDay.Friday => "Piątek"
      case WeekDay.Saturday => "Sobota"
      case WeekDay.Sunday => "Niedziela"
    }
  }
  private def nextDay(day: WeekDay): WeekDay = {
    day match {
      case WeekDay.Monday => WeekDay.Tuesday
      case WeekDay.Tuesday => WeekDay.Wednesday
      case WeekDay.Wednesday => WeekDay.Thursday
      case WeekDay.Thursday => WeekDay.Friday
      case WeekDay.Friday => WeekDay.Saturday
      case WeekDay.Saturday => WeekDay.Sunday
      case WeekDay.Sunday => WeekDay.Monday
    }}
enum WeekDay:
  case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday


//Zadanie 4:
enum Maybe:
  case Nothing
  case Just[A](value: A)

private def safeHead[A](list: List[A]): Maybe = {
    list match {
      case Nil => Maybe.Nothing
      case head :: tail => Maybe.Just(head)
    }
  }


//Zadanie 5:
type SolidFigure = Figure
enum Figure:
  case Cuboid(length:Int,width:Int,height:Int)
  case Cone(radius:Int,height:Int)
  case Sphere(radius:Int)
  case Cylinder(radius:Int,height:Int)
  case Line(length:Int)

private def Volume(figure: SolidFigure): Maybe = {
  figure match {
    case Figure.Cuboid(length, width, height) => Maybe.Just(length * width * height)
    case Figure.Cone(radius, height) =>          Maybe.Just(Math.PI * Math.pow(radius, 2) * height / 3)
    case Figure.Sphere(radius) =>                Maybe.Just(4 * Math.PI * Math.pow(radius, 3) / 3)
    case Figure.Cylinder(radius, height) =>      Maybe.Just(Math.PI * Math.pow(radius, 2) * height)
    case Figure.Line(length) =>                  Maybe.Nothing // a line has no volume
  }
}





