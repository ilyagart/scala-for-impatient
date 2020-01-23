// Chapter 13. Collections
import java.awt.Color
import scala.language.postfixOps
import scala.collection.{SortedSet, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

Iterable(0xFF, 0xFF00, 0xFF0000)
Set(Color.RED, Color.GREEN, Color.BLUE)
Map(Color.RED -> 0xFF0000, Color.GREEN -> 0xFF00, Color.BLUE -> 0xFF)
SortedSet("Hello", "World")

val coll = Seq(1, 1, 2, 3, 5, 8, 13)
val set = coll.toSet
val buffer2 = coll.to(ArrayBuffer)
Seq(1, 2, 3) == (1 to 3) // true
//noinspection ComparingDiffCollectionKinds
Seq(1, 2, 3) == Set(1, 2, 3) // false
//noinspection CorrespondsUnsorted
Seq(1, 2, 3).sameElements(Set(1, 2, 3)) // true
def digitz(n: Int): Set[Int] =
  if (n < 0) digitz(-n)
  else if (n < 10) Set(n)
  else digitz(n / 10) + (n % 10)
digitz(143247980)

val digits1 = List(4, 2)
digits1.head
digits1.tail
digits1.tail.head
digits1.tail.tail
9 :: List(4, 2)
9 :: 4 :: 2 :: Nil // equivalent
9 :: (4 :: (2 :: Nil)) // equivalent
def basicSum(lst: List[Int]): Int =
  if (lst == Nil) 0 else lst.head + basicSum(lst.tail)
// recursion over iterators
def patternMatchingSum(lst: List[Int]): Int = lst match {
  case Nil    => 0
  case h :: t => h + patternMatchingSum(t) // h is lst.head, t is lst.tail
}
val digits2 = Set(1, 7, 2, 9)
val primes = Set(2, 3, 5, 7)
digits2 & primes
digits2 &~ primes
digits2 -- primes
digits2 | primes
digits2 ++ primes
Vector(1, 2, 3) :+ 5
1 +: Vector(1, 2, 3)

def ulcase(s: String) = Vector(s.toUpperCase(), s.toLowerCase())
val names = List("Peter", "Paul", "Mary")
names.map(ulcase)
names.flatMap(ulcase)
for (i <- 1 to 10; j <- 1 to i) yield i * j
(1 to 10).flatMap(i => (1 to i).map(j => i * j))
"-3+4".collect { case '+' => 1; case '-' => -1 }
"Mississippi".foldLeft[Map[Char, Int]](Map.empty) { (m, c) =>
  m + (c -> (m.getOrElse(c, 0) + 1))
}
(1 to 10).scanLeft(0)(_ + _)
val prices = List(5.0, 20.0, 9.95)
val quantities = List(10, 2, 1)
//(prices zip prices.map(_ * 2)).max._2
((prices zip quantities) map { p =>
  p._1 * p._2
}).sum
"Scala".indexOf("Scala".map(_.toInt).max.toChar)
"Scala".indexOf("Scala".max)

def numsFrom(n: BigInt): LazyList[BigInt] = n #:: numsFrom(n + 1)
val tenOrMore = numsFrom(10)
tenOrMore.tail.tail.tail
val squares = numsFrom(1).map(x => x * x)
squares.take(5).force // won't be calculated without .force
val palindromicSquares = (1 to 1000000).view
  .map(x => x * x)
  .filter(x => x.toString == x.toString.reverse)
palindromicSquares.take(10).mkString("[", ", ", "]")
