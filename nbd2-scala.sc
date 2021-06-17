
//=================================================================================================

//zad 1
def printTypeOfDay(day: String) = {
  day match {
    case "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday"  => println("Praca")
    case "Saturday" | "Sunday" => println("Weekend")
    case _ => println("Nie ma takiego dnia")
  }
}

printTypeOfDay("Monday")
printTypeOfDay("Friday")
printTypeOfDay("Saturday")
printTypeOfDay("dasdsa")

//=================================================================================================

//zad 2

class KontoBankowe(private var stanKonta: Double) {

  def this() {
    this(0.0)
  }

  def wplata(wartosc: Double): Unit = {
    stanKonta += wartosc
  }

  def wyplata(wartosc: Double): Unit = {
    if(stanKonta - wartosc >= 0) {
      stanKonta -= wartosc;
    } else {
      println("Brak srodkow")
    }
  }

  def zwrocStanKonta() = {
    stanKonta
  }
}

val konto = new KontoBankowe()
println(konto.zwrocStanKonta())
konto.wyplata(100.0)
println(konto.zwrocStanKonta())
konto.wplata(200.0)
println(konto.zwrocStanKonta())
konto.wyplata(100.0)
println(konto.zwrocStanKonta())

val konto2 = new KontoBankowe(1000.0)
println(konto2.zwrocStanKonta())

//====================================================================================================

//zad 3

class Osoba(val imie: String, val nazwisko: String)

val osoba1 = new Osoba("Bartosz", "Waś")
val osoba2 = new Osoba("Marek", "Kowalski")
val osoba3 = new Osoba("John", "Doe")
val osoba4 = new Osoba("J", "D")

def przywitaj(osoba: Osoba) = {
  osoba.imie match {
    case "Bartosz" => "Hello Bartosz"
    case "Marek" => "Hi Marek"
    case "John" => "Cześć John"
    case _ => "Hi"
  }
}

println(przywitaj(osoba1))
println(przywitaj(osoba2))
println(przywitaj(osoba3))
println(przywitaj(osoba4))

//======================================================================================================

//zad 4

def function(number: Int, nestedFunction: Int => Int) = {
  nestedFunction(number)
  nestedFunction(number)
  nestedFunction(number)
}

println(function(10, element => element * 10))

//======================================================================================================

//zad 5

class Pearson(val name: String, val surname: String, val tax: Double)

trait Student extends Pearson {
  override val tax = 0.0
}

trait Employee extends Pearson {
  override val tax = 0.2
  var salary: Double
}

trait Teacher extends Employee {
  override val tax = 0.1
}

val student = new Pearson("pearson1", "surname1", 0.19) with Student

val employee = new Pearson("pearson2", "surname2", 0.19) with Employee {
  override var salary = 3000.0
}

val teacher = new Pearson("pearson3", "surname3", 0.19) with Teacher {
  override var salary = 5000.0
}

val employeeStudent = new Pearson("pearson","surname4", 0.19) with Student with Employee {
  override var salary = 2000.0
}

val studentEmployee = new Pearson("pearson5","surname5", 0.19) with Employee with Student {
  override var salary = 6000.0
}


println("Student " + student.name + " " + student.surname + " " + student.tax)
println("Employee " + employee.name + " " + employee.surname + " " + employee.tax + " " + employee.salary)
println("Teacher " + teacher.name + " " + teacher.surname + " " + teacher.tax + " " + teacher.salary)
println("Employee-Student " + employeeStudent.name + " " + employeeStudent.surname + " " + employeeStudent.tax + " " + employeeStudent.salary)
println("Student-Employee " + studentEmployee.name + " " + studentEmployee.surname + " " + studentEmployee.tax + " " + studentEmployee.salary)

//======================================================================================================================================================================