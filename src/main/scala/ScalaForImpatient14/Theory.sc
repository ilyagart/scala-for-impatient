// Chapter 14. Pattern Matching and Case Classes
import scala.language.postfixOps
val ch: Char = '3'
var digit = 0
var sign = ch match {
  case _ if Character.isDigit(ch) => digit = Character.digit(ch, 10)
  case '+'                        => 1
  case '-'                        => -1
  case _                          => 0
}
digit
val str = "1 + 2 + 4 - 5"
str(8) match {
  case '+'  => sign = 1
  case '-'  => sign = -1
  case `ch` => digit = Character.digit(ch, 10)
  case ch   => digit = Character.digit(ch, 10)
}
digit

// obj match {
//  case x: Int => x
//  case s: String => Integer.parseInt(s)
//  case _: BigInt => Int.MaxValue
//  case _ => 0
//}
// Generic type of `obj` is calculated at Runtime
//In Scala, this form is preferred to using the isInstanceOf operator.

val arr = Array(1, 1000, 100)

arr match {
  case Array(0)            => "0"
  case Array(x, y)         => s"$x $y"
  case Array(0, _*)        => "0 ..."
  case Array(x, rest @ _*) => s"$x ${rest.min}"
  case _                   => "something else"
}

val lst = List(0, 10, 20, 40)

lst match {
  case 0 :: Nil      => "0"
  case x :: y :: Nil => s"$x $y"
  case 0 :: tail     => s"0 ... ${tail.max}"
  case _             => "something else"
}

val pair = (3, 0)
pair match {
  case (0, _) => "0 ..."
  case (y, 0) => s"$y 0"
  case _      => "neither is 0"
}

val (q, r) = BigInt(10) /% 3
val Array(first, second, rest @ _*) = arr

val Array(scala.math.Pi, x) = Array(scala.math.Pi, 2) // variables
// that start with Capital letter are always constants (can't be assigned to other vars)

var x = Array(1, 2, 3)
val 2 = x(1)
val $result: Unit = x(1) match { case 2 => () }

import scala.jdk.CollectionConverters.PropertiesHasAsScala
// Converts Java Properties to a Scala map—just to get an interesting example
for ((k, v) <- System.getProperties.asScala)
  println(s"$k -> $v")

for ((k, "") <- System.getProperties.asScala)
  println(k)
// same as
for ((k, v) <- System.getProperties.asScala if v == "")
  println(k)

abstract class Amount

case class Dollar(value: Double) extends Amount

case class Currency(value: Double, unit: String) extends Amount

case object Nothing extends Amount

val amt: Amount = Currency(14, "Lei")
amt match {
  case Dollar(v)      => s"$$$v"
  case Currency(_, u) => s"Oh noes, I got $u"
  case Nothing        => ""
}

val amt2 = Currency(29.95, "EUR")
val price1 = amt2.copy()
val price2 = amt2.copy(value = 19.95)
val price3 = amt2.copy(unit = "CHF")

amt match {
  case a Currency u =>
    println(a * 2)
    u.concat(" Units")
}
// same as Currency(a, u)

lst match {
  case h :: t =>
    println(h + 1)
    t.max
}
lst.flatMap(i => (i to i + 5).map(_ * 3))

case object +: {
  def unapply[T](input: List[T]): Option[(T, List[T])] =
    if (input.isEmpty) None else Some((input.head, input.tail))
}

1 +: 7 +: 2 +: 9 +: Nil match {
  case first +: second +: rest => first + second + rest.length
} // +: allows us to destructure the list

abstract class Item

case class Article(description: String, price: Double) extends Item

case class Bundle(description: String, discount: Double, items: Item*)
    extends Item

val bundle = Bundle(
  "Father's day special",
  20.0,
  Article("Scala for the Impatient", 39.95),
  Bundle(
    "Anchor Distillery Sampler",
    10.0,
    Article("Old Potrero Straight Rye Whiskey", 79.95),
    Article("Junípero Gin", 32.95)
  )
)
bundle match {
  case Bundle(_, _, Article(descr, _), _*) => println(descr)
}

bundle match {
  case Bundle(_, _, art @ Article(_, _), rest @ _*) =>
    println(art.description)
    println(rest.head)
} // without _* rest would be only 1 next item after Article(_,_)

def price(it: Item): Double = it match {
  case Article(_, p)             => p
  case Bundle(_, disc, its @ _*) => its.map(price).sum - disc
}
price(bundle)
price(Article("SomeArticle", 10))

sealed abstract class TrafficLightColor

case object Red extends TrafficLightColor

case object Yellow extends TrafficLightColor

case object Green extends TrafficLightColor
val color: TrafficLightColor = Yellow
color match {
  case Red    => "stop"
  case Yellow => "hurry up"
  case Green  => "go"
}
val scores =
  Map[String, Array[Int]](
    "John" -> Array(1, 2, 3),
    "Alice" -> Array(10, 11, 4, 6),
    "Mike" -> Array(20, 30, 40)
  )
val optionalAlicesScores = scores.get("Alice")
val alicesScore = scores("Alice")
// Null-safe methods - map, filter, foreach
val biggerScore = alicesScore.map(_ + 1) // Some(score + 1) or None
val acceptableScore = alicesScore.filter(_ > 5) // Some(score) if score > 5 or None
alicesScore.foreach(println) // Prints the score if it exists

optionalAlicesScores match {
  case Some(score) => println(score)
  case None        => println("No score")
}

Option(optionalAlicesScores) // Option(value) is also null-safe

val f: PartialFunction[Char, Int] = { case '+' => 1; case '-' => -1 }
f('-') // Calls f.apply('-'), returns -1
f.isDefinedAt('0') // false
//f('0')  Throws MatchError

"-3+4".collect { f } // Vector(-1, 1)
"-3+4".map { case '+' => 1; case '-' => -1; case _ => 0 } // Vector(-1, 0, 1, 0)

val names = Array("Alice", "Bob", "Carmen")
val scores2 = Map("Alice" -> 10, "Carmen" -> 7)
names.collect(scores2) // Yields Array(10, 7)

val f2: PartialFunction[Char, Int] = { case '+' => 1; case '-' => -1 }
val g = f2.lift // A function with type Char => Option[Int]

def tryCatch[T](b: => T, catcher: PartialFunction[Throwable, T]) =
  try { b } catch catcher

val result = tryCatch(str.toInt,
  { case _: NumberFormatException => -1 })