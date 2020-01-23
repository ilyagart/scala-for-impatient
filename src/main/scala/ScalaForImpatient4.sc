import scala.collection.{SortedMap, mutable}
import scala.language.postfixOps

val scores = Map("Alice" -> 10, "Bob" -> 3, "Cindy" -> 8)
val mutableScores =
  scala.collection.mutable.Map("Alice" -> 10, "Bob" -> 3, "Cindy" -> 8)
val blankMap = scala.collection.mutable.Map[String, Int]()
val bobsScore = scores("Bob")
val bobsScore = if (scores.contains("Bob")) scores("Bob") else 0
val bobsScore = scores.getOrElse("Bob", 0)

val scores1 = scores withDefaultValue 0
scores1("Zelda")
val scores2 = scores withDefault (_.length)
scores2("Zelda")

mutableScores("Bob") = 10
mutableScores("Fred") = 7
mutableScores ++= Map("Bob" -> 11, "Fred" -> 8)
mutableScores -= "Alice"
var newScores = scores ++ Map("Rob" -> 10, "John" -> 7)
newScores -= "Bob"
newScores
newScores = newScores - "Rob"
newScores

scores.keySet

for ((k, v) <- scores) yield (v, k)
for ((k, v) <- scores) yield v -> k
val sortedScores = scala.collection.mutable.SortedMap(
  "Alice" -> 10,
  "Fred" -> 7,
  "Bob" -> 3,
  "Cindy" -> 8
)

val months = scala.collection.mutable.LinkedHashMap(
  "January" -> 1,
  "February" -> 2,
  "March" -> 3,
  "April" -> 4,
  "May" -> 5
)

import scala.jdk.CollectionConverters.{MapHasAsScala, PropertiesHasAsScala}

val javaMap: scala.collection.mutable.Map[String, Int] =
  new java.util.TreeMap[String, Int].asScala

val props: scala.collection.Map[String, String] = System.getProperties.asScala

val t = (1, 3.14, "Fred")
val second = t _2

val (first, second, third) = t
val (first, second, _) = t

"New York".partition(_.isUpper)._1
"New York".filter(_.isUpper)
"New York"
  .charAt("New York".indexWhere(_.isUpper))
  .toString
  .+("New York".charAt("New York".lastIndexWhere(_.isUpper)))
//"New York".foldLeft()
"New York".find(_.isUpper).toString + "New York".findLast(_.isUpper)
"HELLO".map {
  _.toByte + 32 toChar
}

val symbols = Array("<", "-", ">")
val counts = Array(2, 10, 2)
val pairs = symbols zip counts
for ((s, n) <- pairs) print(s * n)
//(symbols zip counts) toMap
pairs toMap

val gizmos = Map[String, Double](
  "spongebob" -> 11,
  "patrick" -> 5,
  "mrCrabs" -> 9,
  "larry" -> 25,
  "sandy" -> 3,
  "plankton" -> 1.8
)
val gizmosDiscounted = for ((k, v) <- gizmos) yield (k, v * 0.9)

val path = getClass.getClassLoader.getResourceAsStream("123.txt")

val in = new java.util.Scanner(path)

val mutableWords = new mutable.HashMap[String, Int]
while (in.hasNext) {
  val w = in.next
  if (mutableWords.contains(w)) mutableWords(w) += 1
  else mutableWords(w) = 1
  //  println(w)
}
mutableWords foreach { case (k, v) => println(s"$k $v") }

val in2 = new java.util.Scanner(path)

var immutableWords = Map[String, Int]()
while (in2.hasNext) {
  val w = in2.next
  immutableWords = immutableWords + (w -> (immutableWords.getOrElse(w, 0) + 1))
  //  println(w)
}
immutableWords foreach { case (k, v) => println(s"$k $v") }

val in3 = new java.util.Scanner(path)

var sortedWords = SortedMap[String, Int]()
while (in3.hasNext) {
  val w = in3.next
  sortedWords = sortedWords + (w -> (sortedWords.getOrElse(w, 0) + 1))
  //  println(w)
}
sortedWords

sortedWords foreach { case (k, v) => println(s"$k $v") }

val in4 = new java.util.Scanner(path)

val treeMap = new java.util.TreeMap[String, Int]
val scalaTreeMap =
  scala.jdk.CollectionConverters.MapHasAsScala(treeMap).asScala
while (in4.hasNext) {
  val w = in4.next
  if (scalaTreeMap contains w) scalaTreeMap(w) += 1
  else scalaTreeMap(w) = 1
}

scalaTreeMap foreach { case (k, v) => println(s"$k $v") }

val linkedMap = mutable.LinkedHashMap(
  "Monday" -> java.util.Calendar.MONDAY,
  "Tuesday" -> java.util.Calendar.TUESDAY,
  "Wednesday" -> java.util.Calendar.WEDNESDAY,
  "Thursday" -> java.util.Calendar.THURSDAY,
  "Friday" -> java.util.Calendar.FRIDAY,
  "Saturday" -> java.util.Calendar.SATURDAY,
  "Sunday" -> java.util.Calendar.SUNDAY
)
println(linkedMap)

val properties = System.getProperties.asScala
val maxKeyLength = properties.keySet.map(_.length).max
properties.foreach(
  x => print(s"${x._1}" + " " * (maxKeyLength - x._1.length) + s"| ${x._2}\n")
)

def minmax(values: Array[Int]): (Int, Int) = (values.min, values.max)
val someArray = Array(1, 5, 43, 1, 2, 3, 3, 5, 6, 23, -6, 23)
minmax(someArray)

def lteqgt(values: Array[Int], v: Int): (Int, Int, Int) =
  (values count (_ < v), values count (_ == v), values count (_ > v))

lteqgt(someArray, 3)

"Hello".zip("World")

val nextNum = "012345678" zip "123456789" toMap

nextNum('1')

