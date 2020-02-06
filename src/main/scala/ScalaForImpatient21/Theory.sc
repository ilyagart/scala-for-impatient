import java.io.File

import scala.io.Source
import scala.math.abs

class Fraction(n: Int, d: Int) {
  val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
  val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)
  override def toString = s"$num/$den"
  def sign(a: Int): Int = if (a > 0) 1 else if (a < 0) -1 else 0
  def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)
  def +(that: Fraction): Fraction =
    new Fraction(num * that.den + that.num * den, den * that.den)
  def -(that: Fraction): Fraction = this + -that
  def *(that: Fraction): Fraction =
    new Fraction(num * that.num, den * that.den)
  def /(that: Fraction): Fraction =
    new Fraction(num * that.den, that.num * den)
  def unary_- = new Fraction(-n, d)
}

object Fraction {
  def apply(n: Int, d: Int) = new Fraction(n, d)
}

implicit class RichFile(val from: File) extends AnyVal {
  def read = Source.fromFile(from.getPath).mkString
}
//val contents = new File("README").read

object FractionConversions {
  implicit def fraction2Double(f: Fraction): Double = f.num * 1.0 / f.den
  implicit def int2Fraction(n: Int): Fraction = Fraction(n, 1)
}
import FractionConversions._
// implicits can be replaced by each other depending on import/explicit call
val result2 = 3 * Fraction(4, 5) //implicit call after import
val result3 = int2Fraction(3) * Fraction(4, 5) //explicit call
import FractionConversions.{
  fraction2Double => _,
  _
} //imports everything except fraction2Double
val result4 = 3 * Fraction(4, 5)
3.den // calls int2Fraction
Fraction(4, 5) * 3 //calls int2Fraction since Fraction can only * Fraction, not Int
//class Artem {
//  val ban = s"permaban $this"
//}
//
//implicit def artem2Ban(o: Any): Artem = new Artem
//"kek".ban
//2.ban
//1.0.ban
//true.ban
//0xffffffffL.ban
//"Any value in this universe".ban

case class Delimiters(left: String, right: String)

def quote(what: String)(implicit delims: Delimiters) =
  delims.left + what + delims.right

object FrenchPunctuation {
  implicit val quoteDelimiters: Delimiters = Delimiters("[oui ", " oui]")
}
quote("Bonjour le monde")(Delimiters("<<", ">>"))
quote("yes")(FrenchPunctuation.quoteDelimiters)
//or just
import FrenchPunctuation._
quote("oui oui sehr gut")

//def smaller[T](a: T, b: T) = if (a < b) a else b doesn't work

def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]) =
  if (order(a) < b) a else b

def smaller2[T](a: T, b: T)(implicit order: T => Ordered[T]) =
  if (a < b) a else b // Calls order(a) < b if a doesn't have a < operator

smaller2(40, 2)
smaller2("Hello", "world")

class OrderedPair[T: Ordering](val first: T, val second: T) {
  def smaller(implicit ord: Ordering[T]) =
    if (ord.compare(first, second) < 0) first else second
  /*
  another way:
  def smaller =
if (implicitly[Ordering[T]].compare(first, second) < 0) first else second
   */
}
def implicitly[T](implicit e: T) = e

new OrderedPair(40, 2)

abstract class <:<[-From, +To] extends (From => To)
object <:< {
  implicit def conforms[A]: A <:< A = (x: A) => x
}

implicitly[String <:< AnyRef]