// 1
3 + 4 -> 5 // ((3 + 4) -> 5) // default precedence rules are fine here
3 -> (4 + 5) // ((3 -> 4) + 5) default precedence rules give compilation error,
// -> and + have same priority

// 2
//Scala library designers designed pow that way so it has higher precedence than ** '*',
// '^' operator already exists for BigInt and it does the bitwise exclusive-or operation

// 3
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.math._

class Fraction(n: Int, d: Int) {
  private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
  private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)
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
Fraction(1, 2) + Fraction(3, 4)
Fraction(1, 2) - Fraction(3, 4)
Fraction(1, 2) * Fraction(3, 4)
Fraction(1, 2) / Fraction(3, 4)
-Fraction(1, 2)

// 4
class Money(d: Int, c: Int) {
  if (d < 0) throw new IllegalArgumentException("d: " + d)
  if (c < 0) throw new IllegalArgumentException("c: " + c)

  val dollars: Int = if (c > 99) d + (c / 100) else d
  val cents: Int = if (c > 99) c % 100 else c

  def +(that: Money) = new Money(dollars + that.dollars, cents + that.cents)
  def -(that: Money) = {
    val d: Int = dollars - that.dollars
    val c: Int = cents - that.cents
    if (c < 0) Money(d - 1, c + 100)
    else Money(d, c)
  }
  def ==(that: Money) = dollars == that.dollars && cents == that.cents
  def <(that: Money) =
    dollars < that.dollars || dollars == that.dollars && cents < that.cents
  override def toString = s"[$$$dollars, c$cents]"
}

object Money {
  def apply(d: Int, c: Int) = new Money(d, c)
}
Money(1, 75) + Money(0, 50) == Money(2, 25)
Money(3, 0) - Money(1, 99)

// 5
// Provide operators that construct an HTML table. For example,
//Click here to view code image
//Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
//should produce
//Click here to view code image
//<table><tr><td>Java</td><td>Scala</td></tr><tr><td>Gosling...
class Table private {
  val table = new ListBuffer[ListBuffer[String]]

  def |(cell: String): Table = {
    if (table.isEmpty) {
      table += new ListBuffer[String]
    }

    table.last += cell
    this
  }

  def ||(cell: String): Table = {
    table += new ListBuffer[String]
    table.last += cell
    this
  }

  def toHtml: String = {
    val sb = new StringBuilder("<table>")
    for (row <- table) {
      sb ++= "<tr>"
      for (cell <- row) {
        sb ++= "<td>"
        sb ++= cell
        sb ++= "</td>"
      }

      sb ++= "</tr>"
    }

    sb ++= "</table>"
    sb.toString()
  }

  override def toString = table.mkString("\n")
}

object Table {
  def apply() = new Table
}
val table = Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
table.toHtml

// 6
class ASCIIArt(val rows: List[String] = Nil) {

  def +(row: String): ASCIIArt = new ASCIIArt(rows :+ row)

  def |(other: ASCIIArt): ASCIIArt = {
    val padLen = rows.foldLeft(0) { (len, s) =>
      if (len > s.length) len
      else s.length
    }
    val result = new ArrayBuffer[String]
    result ++= rows

    val buf = new StringBuilder
    var i = 0
    for (row <- other.rows) {
      if (i >= result.length) {
        result += ""
      }

      buf.clear()
      buf ++= result(i)
      while (buf.length < padLen) {
        buf.append(' ')
      }

      buf ++= row
      result(i) = buf.toString()
      i += 1
    }

    new ASCIIArt(result.toList)
  }

  override def toString = rows.mkString("\n")
}
val art = new ASCIIArt +
  """ /\_/\""" +
  """( ' ' )""" +
  """(  -  )""" +
  """ | | |""" +
  """(__|__)"""
val result = art | new ASCIIArt +
  """    -----""" +
  """  / Hello \""" +
  """ <  Scala |""" +
  """  \ Coder /""" +
  """    -----"""

// 7
class BitSequence {
  private var bits: Long = 0

  def apply(i: Int): Int = get(mask(i))

  def update(i: Int, bit: Int): Unit = set(mask(i), bit)

  private def mask(i: Int): Long = 1L << i

  private def get(mask: Long): Int =
    if ((bits & mask) == mask) 1
    else 0

  private def set(mask: Long, bit: Int): Unit =
    if (bit == 0) bits &= ~mask
    else bits |= mask

  override def toString: String = s"$bits"
}

val i = new BitSequence
i.update(3, 1)
i.update(2, 1)
i.update(1, 0)
i.update(0, 1)

val j = new BitSequence
j(0) = 1
j(1) = 0
j(2) = 1
j
j(0) = 0
j

// 8
class Matrix(private val n: Int, private val values: IndexedSeq[Int]) {
  require(values.length == n * n)
  val matrix: Array[Array[Int]] = Array.ofDim[Int](n, n)
  def +(that: Matrix): Matrix = {
    Matrix.init(n)((row, col) => this(row, col) + that(row, col))
  }
  def *(value: Int): Matrix = {
    Matrix.init(n)((row, col) => this(row, col) * value)
  }
  def *(that: Matrix): Matrix = {
    Matrix.init(n)((row, col) => this(row, col) * that(row, col))
  }
  def apply(row: Int, col: Int): Int = values(row * n + col)
}

object Matrix {
  def apply(n: Int)(data: Int*): Matrix = {
    new Matrix(n, data.toIndexedSeq)
  }
  def init(n: Int)(op: (Int, Int) => Int): Matrix = {
    val data = new Array[Int](n * n)
    for (row <- 0 until n) {
      for (col <- 0 until n) {
        data(row * n + col) = op(row, col)
      }
    }
    new Matrix(n, data)
  }
}
Matrix(2)(1, 2, 3, 4)(1, 1) + Matrix(2)(1, 2, 3, 4)(0, 0)
(Matrix(2)(1, 2, 3, 4) + Matrix(2)(1, 2, 3, 4))(0, 0)
(Matrix(2)(1, 2, 3, 4) + Matrix(2)(1, 2, 3, 4))(1, 1)
(Matrix(2)(1, 2, 3, 4) * 69)(0, 1)
(Matrix(2)(1, 2, 3, 4) * Matrix(2)(1, 2, 3, 4))(1, 0)
/*Matrix(2)(1, 2, 3) java.lang.IllegalArgumentException: requirement failed
//at scala.Predef$.require(Predef.scala:327)
... 32 elided*/

// 9
object PathComponents {
  var fullPath: String = ""
  def unapply: (String, String) = {
    val tokens = fullPath.split('/')
    val file = tokens.last
    val path = fullPath.take(fullPath.indexOf(file))
    (path, file)
  }

  override def toString: String = {
    val path = unapply._1
    val file = unapply._2
    s"[path: $path, file: $file]"
  }
}
PathComponents.fullPath = "/home/cay/readme.txt"
PathComponents

// 10
object PathComponents2 {
  var fullPath: String = ""
  def unapplySeq: Seq[String] = {
    val seq = new ListBuffer[String]
    seq ++= fullPath.split('/')
    seq.toSeq.tail
  }
}
PathComponents2.fullPath = "/home/cay/readme.txt"
PathComponents2.unapplySeq
