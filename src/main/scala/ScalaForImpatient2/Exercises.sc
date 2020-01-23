import java.time.LocalDate

def signum(n: Int): Int =
  if (n == 0) 0
  else if (n > 0) 1
  else -1

var x: Unit = {}
var y = 0
x = y = 1

for (i <- 11 to 0 by -1)
  println(i)

def countdown(n: Int) {
  for (i <- n to 0 by -1)
    println(i)
}
countdown(3)

("Hello" map (i => BigInt(i))).product

object Product {
  def unapply[T: Numeric](xs: Seq[T]) = Some(xs.product)
}

val Product(pro) = for (i <- "Hello") yield BigInt(i)

"Hello".map(BigInt(_)).product

"Hello".foldLeft(1L)(_ * _)

def product(s: String): BigInt = {
  s.foldLeft(1L)(_ * _)
}

def recProduct(s: String): BigInt = {
  if (s.isEmpty) 1
  else
    s.head * recProduct(s.tail)
}
recProduct("Hello")

product("Kek")

def kaia(s: String): String = {
  if (s.isEmpty) s
  else s.last + kaia(s.dropRight(1))
}

def keka(s: String): String = {
  if (s.isEmpty) s
  else keka(s.tail) + s.head
}
kaia("artem") == "artem".reverse
keka("artemka") == "artemka".reverse

def pow(x: Double, n: Int): Double = {
  if (n == 0) 1
  else if (n > 0 && n % 2 == 0) pow(x * x, n / 2)
  else if (n > 0 && n % 2 != 0) x * pow(x, n - 1)
  else 1 / pow(x, -n)
}

pow(5, 0)
pow(5, 2)
pow(5, 3)
pow(5, -3)

implicit class DateInterpolator(val sc: StringContext) extends AnyVal {

  def date(args: Any*): LocalDate = {
    try {
      if (args.length != 3)
        throw new IllegalArgumentException("there aren't three arguments")
      val (year, month, day) = (
        args(0).toString.toInt,
        args(1).toString.toInt,
        args(2).toString.toInt
      )
      for (x <- sc.parts)
        if (x.length > 0 && !x.equals("-"))
          throw new IllegalArgumentException(
            "Date parts aren't separated by dashes"
          )
      LocalDate.of(year, month, day)
    } catch {
      case ex: NumberFormatException =>
        println("Date parts aren't integer")
        throw ex
      case ex: IllegalArgumentException =>
        println("See exception message for detail")
        throw ex
    }
  }
}

val year = 2020
val month = 1
val day = 21

date"$year-$month-$day"