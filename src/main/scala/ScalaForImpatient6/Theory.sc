// Chapter 6. Objects
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

