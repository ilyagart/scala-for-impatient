import scala.collection.{SortedMap, mutable}
import scala.language.postfixOps
import scala.jdk.CollectionConverters.{MapHasAsScala, PropertiesHasAsScala}

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
