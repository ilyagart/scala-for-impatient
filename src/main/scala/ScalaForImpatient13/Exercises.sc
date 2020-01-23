// 1
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

def mapToIndices(s: String): Map[Char, List[Int]] = {
  s.zipWithIndex.groupBy(_._1).map(x => (x._1, x._2.map(_._2).toList))
}
mapToIndices("Mississippi")

// 2
def immutableMapToIndices(s: String): Map[Char, List[Int]] = {
  var map = Map[Char, List[Int]]()
  for (i <- 0 until s.length) {
    val c = s(i)
    map = map.updated(c, map.getOrElse(c, Nil) :+ i)
  }
  map
}

// 3
/*
Write a function that removes every second element from a ListBuffer. Try it two ways.
Call remove(i) for all even i starting at the end of the list. Copy every second element to a
new list. Compare the performance.
   */
def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}

def removeEverySecondElement[A](lb: ListBuffer[A]): ListBuffer[A] = {
  for (i <- lb.length - 1 to 0 by -2)
    lb.remove(i)
  lb
}

time { removeEverySecondElement(ListBuffer(1, 2, 3, 4, 5, 6)) }

def removeEverySecondElement2[A](lb: ListBuffer[A]): ListBuffer[A] = {
  val newList = ListBuffer[A]()
  for (i <- lb.indices by 2) { newList += lb(i) }
  newList
}
time { removeEverySecondElement2(ListBuffer(1, 2, 3, 4, 5, 6)) } // way faster than remove(i)

// 4
def combine(list: Array[String], map: Map[String, Int]): Array[Int] = {
  list.flatMap(map.get)
}
combine(
  Array("Tom", "Fred", "Harry"),
  Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)
)

// 5
def fakeMkString[T](s: Iterable[T], delimiter: Char): String = {
  if (s.isEmpty) ""
  else s.reduceLeft((a: Any, b: T) => a + delimiter.toString + b).toString
}
fakeMkString(Array("kek", "kok", "kak"), '^')
fakeMkString(Array(1, 2, 3), 'k')

// 6
val lst = List(1, 2, 3, 5, 6, 456, 87, 456, 321, 456, 159, 78)
lst.foldLeft(List[Int]())(_ :+ _) // copy by foldLeft
lst.foldLeft(List[Int]())((a, b) => b :: a) // reverse by foldLeft
lst.foldRight(List[Int]())(_ :: _) // copy by foldRight
lst.foldRight(List[Int]())((a, b) => b :+ a) // reverse by foldRight

// 7
def multiply(prices: Iterable[Double],
             quantities: Iterable[Int]): Iterable[Double] = {
  (prices zip quantities) map Function.tupled { _ * _ }
}
val prices = List(5.0, 20.0, 9.95)
val quantities = List(10, 2, 1)
multiply(prices, quantities)

// 8
def make2Dim(ar: Array[Double], col: Int): Array[Array[Double]] = {
  ar.grouped(col).toArray
}
make2Dim(Array(1, 2, 3, 4, 5, 6), 3)
make2Dim(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), 3)

// 9
/*
The Scala compiler transforms a for/yield expression
for (i <- 1 to 10; j <- 1 to i) yield i * j
to invocations of flatMap and map, like this:
(1 to 10).flatMap(i => (1 to i).map(j => i * j))
Explain the use of flatMap. Hint: What is (1 to i).map(j => i * j) when i is 1,
2, 3?
What happens when there are three generators in the for/yield expression?

flatMap allows us to "flatten" the result from collection[collections[Type]]
into initial collection[Type]
   */
(1 to 10).flatMap(i => (1 to i).map(j => i * j))
(1 to 10).flatMap(1 to _)
for (i <- 1 to 10; j <- 1 to i; k <- 1 to j) yield i * j * k
(1 to 10).flatMap(i => (1 to i).flatMap(j => (1 to j).map(k => i * j * k)))

// 10
java.util.TimeZone.getAvailableIDs
  .filter(_.contains('/'))
  .groupBy(tz => tz.takeWhile(_ != '/'))
  .toList
  .sortWith(_._2.length > _._2.length)
  .head
  ._1

java.util.TimeZone.getAvailableIDs
  .groupBy(_.takeWhile(_ != '/'))
  .toList
  .max(Ordering[Int].on[(_, Array[String])](_._2.length))
  ._1

// 11
// in HarryHacker.scala