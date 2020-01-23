// Chapter 4. Maps and Tuples
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

