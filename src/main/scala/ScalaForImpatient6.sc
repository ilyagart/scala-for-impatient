import scala.language.postfixOps

class Account {
  val id = Account.newUniqueNumber()
  private var balance = 0.0

  def deposit(amount: Double) {
    balance += amount
  }
}

object Account {
  private var lastNumber = 0

  def newUniqueNumber() = {
    lastNumber += 1
    lastNumber
  }
}

abstract class UndoableAction(val description: String) {
  def undo(): Unit

  def redo(): Unit
}

object DoNothingAction extends UndoableAction("Do nothing") {
  override def undo() {}

  override def redo() {}
}

val actions = Map("open" -> DoNothingAction, "save" -> DoNothingAction)

class AccounTT private (val id: Int, initialBalance: Double) {
  private var balance = initialBalance
}

object AccounTT { // The companion object
  def apply(initialBalance: Double) =
    new AccounTT(1, initialBalance)
}

val acct = AccounTT(1000.0)

object TrafficLightColor extends Enumeration {
  val Red, Yellow, Green = Value
}

import TrafficLightColor._

val colors = (Red, Yellow, Green)
def doWhat(color: TrafficLightColor.Value) = {
  if (color == Red) "stop"
  else if (color == Yellow) "hurry up"
  else "go"
}
doWhat(Yellow)

for (c <- TrafficLightColor.values) println(s"${c.id}: $c")
TrafficLightColor(0) // Calls Enumeration.apply
TrafficLightColor.withName("Red")

// 1
object Conversions {
  def inchesToCentimeters(inches: Double) = inches * 2.54

  def gallonsToLiters(gallons: Double) = gallons * 3.78541

  def milesToKilometers(miles: Double) = miles * 1.60934
}

// 2
class UnitConversion(value: Double) {
  def convert(d: Double): Double = value * d

  override def toString = s"$value"
}

object inchesToCentimeters extends UnitConversion(2.54) {
  def apply(inches: Double) = new UnitConversion(inches * 2.54)
}

object gallonsToLiters extends UnitConversion(3.78541) {
  def apply(gallons: Double) = new UnitConversion(gallons * 3.78541)
}

object milesToKilometers extends UnitConversion(1.60934) {
  def apply(miles: Double) = new UnitConversion((miles * 1.60934))
}

gallonsToLiters(25)
gallonsToLiters.convert(25)

// 3
object Origin extends java.awt.Point

Origin.setLocation(1, 2)
Origin.getLocation
Origin.getY

//its not that good because Point provides ability to mutate data

// 4
class Point(val x: Double, val y: Double) {
  override def toString: String = s"$x $y"
}

object Point extends Point(0, 0) {
  def apply(x: Int, y: Int) = new Point(x, y)
}

Point(1, 35)

// 5
object Reverse extends App {
  def apply(args: Array[String]): Unit = println(args.reverse.mkString(" "))
}

Reverse(Array("this", "is", "reverse"))

// 6
object CardSuit extends Enumeration {
  val Clubs = Value("♣")
  val Hearts = Value("♥")
  val Diamonds = Value("♦")
  val Spades = Value("♠")
}

import CardSuit._

Hearts

// 7
def isRed(cardSuit: CardSuit.Value): Boolean =
  cardSuit == Hearts || cardSuit == Diamonds
isRed(Hearts)
isRed(Diamonds)
isRed(Spades)
isRed(Clubs)

// 8
object Colors extends Enumeration {
  val Black = Value(0x000000)
  val White = Value(0xffffff)
  val Red = Value(0xff0000)
  val Green = Value(0x00ff00)
  val Blue = Value(0x0000ff)
  val Yellow = Value(0xffff00)
  val Cyan = Value(0x00ffff)
  val Magenta = Value(0xff00ff)
}

import Colors._

println(Black)
println("0x%x".format(Colors.Red.id))
