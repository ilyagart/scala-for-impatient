import scala.beans.BeanProperty
import scala.language.postfixOps
import scala.util.Random

class Counter {
  var value = 0 // You must initialize the field
  def increment(): Unit = value += 1

  // Methods are public by default
  def current = value
}

val myCounter = new Counter
myCounter.increment()
myCounter current

class Person {
  var age = 0
  @BeanProperty var name: String = _

  def this(name: String) { // An auxiliary constructor
    this() // Calls primary constructor
    this.name = name
  }

  def this(name: String, age: Int) { // Another auxiliary constructor
    this(name) // Calls previous auxiliary constructor
    this.age = age
  }

  override def toString = s"Person(age=$age, name=$name)"
}

val fred = new Person
fred.age
fred.age = 25

class PrivatePerson {
  private var privateAge = 0 // Make private and rename
  val getter = 1
  val timeStamp = java.time.Instant.now
  private[this] var noOuterObjectAccess = 10 //

  def attempt = noOuterObjectAccess

  def age = privateAge

  def age_=(newValue: Int) {
    if (newValue > privateAge) privateAge = newValue; // Can't get
  }
}

val artem = new PrivatePerson
artem.age = 25
artem.age = 12
artem.age
artem.getter
artem.timeStamp
artem.attempt

fred.setName("Fred")
(fred name)
fred getName

val p = new Person
val p1 = new Person("Nick")
val p2 = new Person("John", 11)

class PrimaryConstructorPerson(val name: String, val age: Int) { // using primary constructor
  println("Just constructed another person")

  def description = s"$name is $age years old"
}

val young = new PrimaryConstructorPerson("young", 1)
young.description

// EXERCISES
// 1
class Counter {
  var value = 0 // You must initialize the field
  def increment() {
    if (value < Int.MaxValue)
      value += 1
    else value = 0
  } // Methods are public by default
  def current = value
}

val testCounter = new Counter
myCounter.value = Int.MaxValue - 2
myCounter.increment()
myCounter.increment()
myCounter.current
myCounter.increment()
myCounter.current

// 2
class BankAccount(private var _balance: Int) {
  def balance = _balance

  def deposit(amount: Int): Unit = _balance += amount

  def withdraw(amount: Int): Unit =
    if (amount < _balance) _balance -= amount
    else
      throw new Error("not enough money :P")

}

val account = new BankAccount(10000)
account.balance
account.deposit(25)
account.balance
account.withdraw(36)
account.balance

// 3
class Time(val hours: Double, val minutes: Double) {
  def before(other: Time): Boolean =
    hours < other.hours ||
      hours == other.hours && minutes < other.minutes

  override def toString = s"Time(hours=$hours, minutes=$minutes)"
}

val now = new Time(
  java.time.LocalDateTime.now().getHour,
  java.time.LocalDateTime.now().getMinute
)
val someRandTime = new Time(Random.nextInt(23), Random.nextInt(60))
now.before(someRandTime)

// 4
class Time2(hrs: Int, min: Int) {
  private val _time = hrs * 60 + min

  def before(other: Time2) = _time < other._time
}

// 5
class Student(@BeanProperty var name: String, @BeanProperty var id: Long)

val student = new Student("John", 1)
student.setId(2)
student.getId

// 6
class Person(var name: String = "", var age: Int = 0) {
  if (age < 0) age = 0

  override def toString = s"Person($age)"
}

val artemka = new Person(name = "ban", age = -2)

// 7
class FullNamePerson(name: String) {
  val Array(firstName, lastName) = name.split(' ')
}

val alex = new FullNamePerson("Alex Smith")
alex.firstName
alex.lastName

// 8
class Car(val manufacturer: String,
          val modelName: String,
          val modelYear: Int,
          var licensePlate: String) {
  def this(manufacturer: String, modelName: String, licensePlate: String) {
    this(manufacturer, modelName, -1, licensePlate)
  }

  def this(manufacturer: String, modelName: String, modelYear: Int) {
    this(manufacturer, modelName, modelYear, "")
  }

  def this(manufacturer: String, modelName: String) {
    this(manufacturer, modelName, -1, "")
  }

  override def toString =
    s"Car(manufacturer=$manufacturer, modelName=$modelName, modelYear=$modelYear, licensePlate=$licensePlate)"
}

new Car("Toyota", "Prius")
new Car("Toyota", "Prius", 2020)
new Car("Toyota", "Prius", "abc 123")
new Car("Toyota", "Prius", 2020, "abc 123")

//Using default parameters for primary constructor
class ModifiedCar(val manufacturer: String,
                  val modelName: String,
                  val modelYear: Int = -1,
                  var licensePlate: String = "") {

  override def toString =
    s"Car(manufacturer=$manufacturer, modelName=$modelName, modelYear=$modelYear, licensePlate=$licensePlate)"
}

new Car("Toyota", "Prius")
new Car("Toyota", "Prius", 2020)
new Car("Toyota", "Prius", "abc 123")
new Car("Toyota", "Prius", 2020, "abc 123")

// 9
//public class JCar {
//  private String manufacturer;
//  private String modelName;
//  private int modelYear = -1;
//  private String licensePlate = "";
//
//  public JCar(String manufacturer, String modelName, int modelYear, String licensePlate) {
//    this.manufacturer = manufacturer;
//    this.modelName = modelName;
//    this.modelYear = modelYear;
//    this.licensePlate = licensePlate;
//  }
//
//  public JCar(String manufacturer, String modelName, int modelYear) {
//    this.manufacturer = manufacturer;
//    this.modelName = modelName;
//    this.modelYear = modelYear;
//  }
//
//  public JCar(String manufacturer, String modelName, String licensePlate) {
//    this.manufacturer = manufacturer;
//    this.modelName = modelName;
//    this.licensePlate = licensePlate;
//  }
//
//  public JCar(String manufacturer, String modelName) {
//    this.manufacturer = manufacturer;
//    this.modelName = modelName;
//  }
//
//  public String getLicensePlate() {
//    return licensePlate;
//  }
//
//  public void setLicensePlate(String licensePlate) {
//    this.licensePlate = licensePlate;
//  }
//
//  public String getManufacturer() {
//    return manufacturer;
//  }
//
//  public String getModelName() {
//    return modelName;
//  }
//
//  public int getModelYear() {
//    return modelYear;
//  }
//
//  @Override
//  public String toString() {
//    return "[" + manufacturer + ", " + modelName + ", " + modelYear + ", '" + licensePlate + "']";
//  }
//}

// 10
class Employee(val name: String, var salary: Double) {
  def this() {
    this("John Q. Public", 0.0)
  }
}

class EmployeeRewritten(val name: String = "John Q. Public",
                        val salary: Double = 0.0)