import java.awt
import scala.math.abs
// 1
/**
How does -> work? That is, how can "Hello" -> 42 and 42 -> "Hello" be pairs
("Hello", 42) and (42, "Hello")? Hint: Predef.ArrowAssoc.

implicit final class ArrowAssoc[A](private val self: A) extends AnyVal {
    @inline def -> [B](y: B): (A, B) = (self, y)
    @deprecated("Use `->` instead. If you still wish to display it as one character, consider using a font with programming ligatures such as Fira Code.", "2.13.0")
    def →[B](y: B): (A, B) = ->(y)
  }
    */
"Hello" -> 42

// 2
implicit class RichMathOps(val self: Int) {
  def +%(percent: Int): Int = self + ((self * percent) / 100d).toInt
}

120 +% 10

// 3
implicit class SecretFactorial(private val self: Int) {
  def unary_! : BigInt = {
    @scala.annotation.tailrec
    def factorial(acc: BigInt, n: Int): BigInt = {
      if (n == 1) acc
      else factorial(acc * n, n - 1)
    }
    factorial(1, self)
  }
}

!5
!100

// 5
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

implicit class RichFraction(val self: Fraction)
    extends AnyVal
    with Ordered[Fraction] {
  def compare(that: Fraction): Int = (self - that).num
}

def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]) =
  if (a < b) a else b

smaller(Fraction(1, 7), Fraction(2, 9))

// 6
implicit class SneakyComparison(self: awt.Point) extends Ordered[awt.Point] {
  def compare(that: awt.Point) =
    if (self.x == that.x) self.y compare that.y
    else self.x compare that.x
}

val point1 = new awt.Point(1, 100)
val point2 = new awt.Point(2, 1)
val point3 = new awt.Point(2, -100)
point1 compare point2 // -1
point1 compare point3 // -1
point2 compare point3 // 1

// 7
/**
Continue the previous exercise, comparing two points according to their distance to the origin.
How can you switch between the two orderings?

can switch between by:
 1. "deactivating" one of the imports
 2. using explicit conversion
 3. importing appropriate class
    */
/**
implicit class SneakierComparison(self: awt.Point)
    extends Ordered[awt.Point] {
  import scala.math._
  def compare(that: awt.Point) =
    sqrt(pow(self.x, 2) + pow(self.y, 2)) compare sqrt(
      pow(that.x, 2) + pow(that.y, 2)
    )
}
    */
val point4 = new awt.Point(1, 100)
val point5 = new awt.Point(2, 1)
val point6 = new awt.Point(2, -100)
point4 compare point5 // 1
point4 compare point6 // -1
point5 compare point6 // -1

// 8
case class Delimiters(left: String, right: String)

object FrenchPunctuation {
  implicit val quoteDelimiters: Delimiters = Delimiters("[oui ", " oui]")
}
import FrenchPunctuation._
implicitly[Delimiters]
//implicitly[Ordered[Fraction]] Error:(127, 11) could not find implicit value for parameter e: Ordered[Fraction]
implicitly[Ordering[Fraction]]

/**
    * For the second case, `implicitly[Ordered[Fraction]]`, it didn't work, since there is no
    * appropriate implicit value defined. And our `implicit class RichFraction` is not suitable
    * since it requires an input parameter.
    */
// 9 TODO
/**
    * Explain why Ordering is a type class and why Ordered is not.
    */
// 10
/**
    *Generalize the average method in Section 21.8, “Type Classes,” on page 331 to a Seq[T].
    */
trait NumberLike[T] {
  def plus(x: T, y: T): T
  def divideBy(x: T, n: Int): T
}

object NumberLike {
  implicit object NumberLikeDouble extends NumberLike[Double] {
    def plus(x: Double, y: Double) = x + y
    def divideBy(x: Double, n: Int) = x / n
  }
  implicit object NumberLikeBigDecimal extends NumberLike[BigDecimal] {
    def plus(x: BigDecimal, y: BigDecimal) = x + y
    def divideBy(x: BigDecimal, n: Int) = x / n
  }
}

trait SeqSecretOps[T] {
  def avg(xs: Seq[T]): Option[T]
}

object SeqSecretOps {
  implicit object SeqLikeSecretSeq extends SeqSecretOps[Int] {
    def avg(xs: Seq[Int]): Option[Int] =
      if (xs.nonEmpty) Some(xs.sum / xs.length) else None
  }
}

def average[T](x: T, y: T)(implicit ev: NumberLike[T]) =
  ev.divideBy(ev.plus(x, y), 2)

def average2[T: NumberLike](x: T, y: T) = {
  val ev = implicitly[NumberLike[T]]
  ev.divideBy(ev.plus(x, y), 2)
}

def bestAverage[T](xs: Seq[T])(implicit ev: SeqSecretOps[T]): Option[T] =
  ev.avg(xs)

bestAverage(Seq(1, 2, 3, 4, 4, 100))

