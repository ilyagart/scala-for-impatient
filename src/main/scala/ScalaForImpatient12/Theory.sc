// Chapter 12. Higher-Order Functions
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton
import scala.math._

val num = 3.14
val fun = ceil _
fun(num)
val f1 = (_: String).charAt(_: Int) //same as
val f2: (String, Int) => Char = _.charAt(_)
f1("abcdef", 2)
f2("abcdef", 2)
val f3: (String, Int) => Char = _(_)
f3("12345", 3)

Array(3.14, 1.42, 2.0) map fun
val triple = (x: Double) => 3 * x
Array(3.14, 1.42, 2.0) map (3 * _)
Array(3.14, 1.42, 2.0) map triple
def valueAtOneQuarter(f: Double => Double) = f(0.25)
valueAtOneQuarter(triple)
def mulBy(factor: Double) = (x: Double) => factor * x
val quintuple = mulBy(5)
quintuple(20)
valueAtOneQuarter(mulBy(100))
valueAtOneQuarter(x => 3 * x)
valueAtOneQuarter(_ * 3)
val fancySubstring: (String, Int, Int) => String =
  _.substring(_, _)
val sameFancySubstring = (_: String).substring(_: Int, _: Int)
val veryFancySubstringAsVal = (s: String, i1: Int, i2: Int) =>
  s.substring(i1, i2)
def veryFancySubStringAsDef(s: String, i1: Int, i2: Int): String =
  s.substring(i1, i2)
val test = "12345678910111213141516..."
fancySubstring(test, 1, 9)
sameFancySubstring(test, 1, 9)
veryFancySubstringAsVal(test, 1, 9)
veryFancySubStringAsDef(test, 1, 9)

(1 to 9) map (_ * 0.1)
(1 to 9) map (" ! " * _) foreach println
(1 to 9) filter (_ % 2 == 0)
(1 to 9).product //same as (1 to 9).reduceLeft(_ * _)
"Mary had a little lamb"
  .split(" ")
  .sortWith((s1, _) => s1.contains("a"))

var counter = 0
val button = new JButton("Increment")
button.addActionListener((_: ActionEvent) => {
  counter += 1
})

button.addActionListener(_ => counter += 1)

val listener: ActionListener = _ => println(counter)
button.addActionListener(listener) // Ok

val exit = (_: ActionEvent) => if (counter > 9) System.exit(0)
button.addActionListener(exit(_))

val mul = (x: Int, y: Int) => x * y
val mulOneAtATime = (x: Int) => (y: Int) => x * y
mul(4, 5)
mulOneAtATime(4)(5)

def sneakyMulOneAtATime(x: Int)(y: Int): Int = x * y
sneakyMulOneAtATime(4)(5)

val a = Array("HeLLo", "World")
val b = Array("hello", "world")
a.corresponds(b)(_.equalsIgnoreCase(_))

def runInThread(block: => Unit) {
  new Thread {
    override def run() { block }
  }.start()
}

@scala.annotation.tailrec
def until(condition: => Boolean)(block: => Unit) {
  if (!condition) {
    block
    until(condition)(block)
  }
}
var s = "ArtemIsFinallyBanned"
until(s == "Artem") {
  println(s)
  s = s.dropRight(1)
}
