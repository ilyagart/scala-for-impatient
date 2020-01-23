import java.time.LocalTime

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

16.toHexString
"bar".take(2)
100 * scala.math.Pi
val cond: (Int, Int) => Boolean = (a, b) => a < b

def insert(x: Int, xs: List[Int]): List[Int] =
  xs match {
    case List() =>
      x ::
        Nil

    case y :: ys =>
      if (cond(x, y)) x :: y :: ys
      else y :: insert(x, ys)
  }
insert(2, 1 :: 3 :: Nil)
insert(1, 2 :: 3 :: Nil)
insert(3, 1 :: 2 :: Nil)

abstract class Reducer(init: Int) {
  def combine(x: Int, y: Int): Int
  def reduce(xs: List[Int]): Int =
    xs match {
      case Nil     => init
      case y :: ys => combine(y, reduce(ys))
    }
}

object Product extends Reducer(1) {
  def combine(x: Int, y: Int): Int = x * y
}

object Sum extends Reducer(0) {
  def combine(x: Int, y: Int): Int = x + y
}

val nums = List(1, 2, 3, 4)

Product.reduce(nums)
Sum.reduce(nums)
nums.combinations(3).foreach(println)

val builder = new StringBuilder

val x = { builder += 'x'; 1 }
lazy val y = { builder += 'y'; 2 }
def z = { builder += 'z'; 3 }

z + y + z + y
builder.result

class Rational(x: Int, y: Int) {

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  lazy val numer: Int = x / g
  lazy val denom: Int = y / g
}

val compareRationals: (Rational, Rational) => Int =
  (a: Rational, b: Rational) =>
    (a, b) match {
      case _ if a.numer * b.denom > b.numer * a.denom  => 1
      case _ if a.numer * b.denom == b.numer * a.denom => 0
      case _ if a.numer * b.denom < b.numer * a.denom  => -1
      case _                                           => throw new IllegalArgumentException
  }

//val compareRationals: (Rational, Rational) => Int = (a, b) =>
//  if (a.numer * b.denom < b.numer * a.denom) -1
//  else if (a.numer * b.denom > b.numer * a.denom) 1
//  else 0

implicit val rationalOrder: Ordering[Rational] = (x: Rational, y: Rational) =>
  compareRationals(x, y)

val half = new Rational(1, 2)
val third = new Rational(1, 3)
val fourth = new Rational(1, 4)
val rationals = List(third, half, fourth)

compareRationals(fourth, third)
rationals sorted rationalOrder

//val comparePopo: (String, String) => Int = (s1, s2) =>
//  (s1: String, s2: String) match {
//    case (_, _) if s1.length > s2.length  => 1
//    case (_, _) if s1.length < s2.length  => -1
//    case (_, _) if s1.length == s2.length => 0
//}
//def comparePopo(s1: String, s2: String): Int = (s1: String, s2: String) match {
//  case (_, _) if s1.length > s2.length  => 1
//  case (_, _) if s1.length < s2.length  => -1
//  case (_, _) if s1.length == s2.length => 0
//}
def comparePopo(arg1: Any, arg2: Any): Int = (arg1, arg2) match {
  case (s1: String, s2: String) if s1.length > s2.length  => 1
  case (s1: String, s2: String) if s1.length < s2.length  => -1
  case (s1: String, s2: String) if s1.length == s2.length => 0
  case _                                                  => -69
}
def compareSmth(a: Any, b: Any): Int = {
  (a: Any, b: Any) match {
    case (s: String, s1: String)   => println("Strings"); s compare s1
    case (d: Int, d1: Int)         => println("Ints"); d compare d1
    case (d: Float, d1: Float)     => println("Floats"); d compare d1
    case (d: Long, d1: Long)       => println("Longs"); d compare d1
    case (b: Boolean, b1: Boolean) => println("Booleans"); b compare b1
    case _                         => println("Not implemented"); -1
  }
}
def compareAnyType[T](o1: T, o2: T)(implicit ord: Ordering[T]) =
  ord.compare(o1, o2)
//comparePopo("123", "456")
//comparePopo("1234", "456")
//comparePopo("1234", "45678")
//comparePopo("123456", "45678")
//comparePopo("123456", "45678910")
//comparePopo(1, 2)
compareSmth(1, 2)
compareSmth(1, 1)
compareSmth(1L, 2L)
compareSmth(1L, 1L)
compareSmth(1.0f, 2.0f)
compareSmth(1.0f, 1.0f)
compareSmth("lol", "kek")
compareSmth("321", "321")
compareSmth(false, true)
compareSmth(false, false)
compareAnyType(1, 2)
compareAnyType(1, 1)
compareAnyType(1L, 2L)
compareAnyType(1L, 1L)
compareAnyType(1.0f, 2.0f)
compareAnyType(1.0f, 1.0f)
compareAnyType("lol", "kek")
compareAnyType("321", "321")
compareAnyType(false, true)
compareAnyType(false, false)

def maybeItWillReturnSomething(flag: Boolean): Option[String] = {
  if (flag) Some("Found value") else None
}
val value1 = maybeItWillReturnSomething(true)
val value2 = maybeItWillReturnSomething(false)

value1 getOrElse "No value"
value2 getOrElse "No value"
value2 getOrElse {
  "default function"
}
import ExecutionContext.Implicits.global
val f2 = Future {
  if (LocalTime.now.getHour > 12)
    throw new Exception("too late")
  42
}
f2
import scala.concurrent.duration._
val f = Future { Thread.sleep(10000); 42 }
//val result = Await.result(f, 10.seconds)
val t = Try(f)

t match {
  case Success(v)  => println(s"The answer is $v")
  case Failure(ex) => println(ex.getMessage)
}
if (t.isSuccess) println(s"The answer is ${t.get}")
if (t.isFailure) println(t.failed.get.getMessage)
val str = ""
val result = Try(str.toInt)
val f3 = Future {
  Thread.sleep(10000)
  if (scala.util.Random.nextInt < 0.5) throw new Exception
  42
}
f3.onComplete {
  case Success(v)  => println(s"The answer is $v")
  case Failure(ex) => println(ex.getMessage)
}

def future1 = Future {
//  Thread.sleep(50)
  if (scala.util.Random.nextInt < 0.5) throw new Exception
  42
}
def future2 = Future {
//  Thread.sleep(50)
  if (scala.util.Random.nextInt < 0.5) throw new Exception
  42
}
val combined = for (n1 <- future1; n2 <- future2) yield n1 + n2
combined.foreach(n => println(s"Result: $n"))
val zipCombined = future1.zipWith(future2)(_ + _)