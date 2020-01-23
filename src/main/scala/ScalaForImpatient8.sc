class Person(val name: String) {
  override def toString = s"${getClass.getName}[name=$name]"
}

class SecretAgent(codename: String) extends Person(codename) {
  override val name = "secret" // Don't want to reveal name ...
  override val toString = "secret" // ... or class name
}

val pleb: Person { def greeting: String } = new Person("random") {
  def greeting = "Greetings, Earthling! My name is Fred."
}
val neo = new SecretAgent("the one")

pleb.name
neo.name

def meet(p: Person { def greeting: String }) {
  println(s"${p.name} says: ${p.greeting}")
}
//object of structural type

meet(pleb)

abstract class Abstraction {
  val id: Int
  var name: String
}

val abstraction = new Abstraction {
  val id: Int = 15
  var name: String = "kek"
}
abstraction.id

def show(o: Any) { println(s"${o.getClass}: $o") }
show(3)
show(3, 4, 5)

// 1
class BankAccount(initialBalance: Double) {
  private var balance = initialBalance
  def currentBalance = balance
  def deposit(amount: Double) = { balance += amount; balance }
  def withdraw(amount: Double) = { balance -= amount; balance }

  override def toString = s"BankAccount(balance=$$$balance)"
}

class CheckingAccount(initialBalance: Double)
    extends BankAccount(initialBalance) {
  override def deposit(amount: Double): Double = super.deposit(amount - 1)

  override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
}

val account = new CheckingAccount(100)
account.deposit(1)
account.withdraw(1)
account.withdraw(1)

// 2
class SavingsAccount(initialBalance: Double,
                     val charges: Double,
                     val interest: Double)
    extends BankAccount(initialBalance) {
  require(interest >= 0 && interest <= 1)
  val maxFreeActions = 3
  private[this] var counter = 0
  def earnMonthlyInterest() {
    counter = 0
    super.deposit(currentBalance * interest)
  }

  override def deposit(amount: Double): Double = {
    counter += 1
    super.deposit(amount - (if (counter > maxFreeActions) charges else 0))
  }

  override def withdraw(amount: Double): Double = {
    counter += 1
    super.withdraw(amount + (if (counter > maxFreeActions) charges else 0))
  }
}

val account2 = new SavingsAccount(100, 5, 0.1) //> balance = 100
account2.deposit(10) //> balance = 110
account2.deposit(10) //> balance = 120
account2.withdraw(20) //> balance = 100
account2.earnMonthlyInterest() //> balance = 110
account2.withdraw(5) //> balance = 105
account2.deposit(15) //> balance = 120
account2.earnMonthlyInterest() //> balance = 132.0
account2.currentBalance

// 3
class Vehicle(val brand: String = "Ford") {

  def honk { println("Tuut, tuut!") }
}

class Car(val modelName: String = "Mustang") extends Vehicle

val myCar = new Car()
myCar.honk
myCar.brand.appended(' ').concat(myCar.modelName)

// 4
abstract class Item {
  def price: Double
  def description: String
}

class SimpleItem(val price: Double, val description: String) extends Item

class Bundle(initItems: SimpleItem*) extends Item {
  val items = initItems.toArray
  def price = items.map(_.price).sum

  def description = items.map(_.description).mkString(",")

  def add(item: SimpleItem) = items.appended(item)
}

val item = new SimpleItem(10, "Potato")
val item2 = new SimpleItem(10000, "Cucumber")
val item3 = new SimpleItem(4, "Artem")
val item4 = new SimpleItem(3, "Thermos")
val item5 = new SimpleItem(5, "Reality")
val item6 = new SimpleItem(6, "Matrix")
item.description
item.price
val bundle = new Bundle(item, item2, item3, item4, item5)
bundle.description
bundle.price
bundle.add(item6)

// 5
class Point(val x: Double, val y: Double) {
  def +(that: Point) = new Point(x + that.x, y + that.y)
  def /(div: Double) = new Point(x / div, y / div)
  override def toString: String = s"[x = $x, y = $y]"
}

class LabeledPoint(label: String, x: Double, y: Double) extends Point(x, y) {

  override def toString = s"($label, $x, $y)"
}

new LabeledPoint("Black Thursday", 1929, 230.07)

// 6
abstract class Shape {
  def centerPoint: Point
}

class Rectangle(a: Point, b: Point) extends Shape {
  override def centerPoint: Point = (a + b) / 2
}

class Circle(override val centerPoint: Point, r: Double) extends Shape

new Rectangle(new Point(1, 2), new Point(3, 4)).centerPoint

new Circle(new Point(1, 3), 14).centerPoint

// 7
class Square(x: Int, y: Int, width: Int, height: Int)
    extends java.awt.Rectangle(x, y, width, height) {
  def this() = this(0, 0, 0, 0)
  def this(width: Int) = this(0, 0, width, width)
  def this(x: Int, y: Int, width: Int) = this(x, y, width, width)
  def this(point: Point, width: Int) =
    this(point.x.toInt, point.y.toInt, width, width)
}

new Square()
new Square(width = 15)
new Square(10, 10, 20)
new Square(new Point(10, 10), 20)

// 8
/*
Compiled from "Person.scala"
public class Person {
  private final java.lang.String name;

  public java.lang.String name();
    Code:
       0: aload_0
       1: getfield      #13                 // Field name:Ljava/lang/String;
       4: areturn

  public java.lang.String toString();
    Code:
       0: new           #18                 // class java/lang/StringBuilder
       3: dup
       4: ldc           #19                 // int 7
       6: invokespecial #23                 // Method java/lang/StringBuilder."<init>":(I)V
       9: aload_0
      10: invokevirtual #27                 // Method getClass:()Ljava/lang/Class;
      13: invokevirtual #32                 // Method java/lang/Class.getName:()Ljava/lang/String;
      16: invokevirtual #36                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
      19: ldc           #38                 // String [name=
      21: invokevirtual #36                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
      24: aload_0
      25: invokevirtual #40                 // Method name:()Ljava/lang/String;
      28: invokevirtual #36                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
      31: ldc           #42                 // String ]
      33: invokevirtual #36                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
      36: invokevirtual #44                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
      39: areturn

  public Person(java.lang.String);
    Code:
       0: aload_0
       1: aload_1
       2: putfield      #13                 // Field name:Ljava/lang/String;
       5: aload_0
       6: invokespecial #48                 // Method java/lang/Object."<init>":()V
       9: return
}

public class SecretAgent extends Person {
  private final java.lang.String name;

  private final java.lang.String toString;

  public java.lang.String name();
    Code:
       0: aload_0
       1: getfield      #14                 // Field name:Ljava/lang/String;
       4: areturn

  public java.lang.String toString();
    Code:
       0: aload_0
       1: getfield      #18                 // Field toString:Ljava/lang/String;
       4: areturn

  public SecretAgent(java.lang.String);
    Code:
       0: aload_0
       1: aload_1
       2: invokespecial #23                 // Method Person."<init>":(Ljava/lang/String;)V
       5: aload_0
       6: ldc           #25                 // String secret
       8: putfield      #14                 // Field name:Ljava/lang/String;
      11: aload_0
      12: ldc           #25                 // String secret
      14: putfield      #18                 // Field toString:Ljava/lang/String;
      17: return

}
   */
// 9
class Creature {
  def range: Int = 10
  val env: Array[Int] = new Array[Int](range)
}

class Ant extends Creature {
  override def range: Int = 2
}
new Ant().env
new Ant().range
//def in main class -> override val or def in subclass,
// def -> def env works properly in subclass(0,0), val -> def env is loaded from superclass()
//val in main -> override val in subclass, env is loaded from superclass()

// 11
class ValPoint(private val xy: Long) extends AnyVal {
  def x: Int = (xy >> 32).toInt
  def y: Int = xy.toInt

  override def toString: String = s"[$x, $y]"
}

object ValPoint {
  def apply(x: Int, y: Int): ValPoint = {
    val xy: Long = (x.toLong << 32) | (y & 0xffffffffL)
    new ValPoint(xy)
  }
}

ValPoint(534343254, 5)