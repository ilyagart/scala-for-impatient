// Chapter 11. Operators
val √ = scala.math.sqrt _
√(2)
val `val` = 42
`val`
Thread.`yield`()
1 -> 10
1 to 10

class Artem {
  def unary_!(): Unit = println("ARTEM - MEGABAN")
  def unary_~(): Unit = println("Not ARTEM - BAN")
  def unary_+(): Unit = println("Positive ARTEM - BAN")
  def unary_-(): Unit = println("Negative ARTEM - BAN")
}
val artem1 = new Artem
val artem2 = new Artem
val artem3 = new Artem
val artem4 = new Artem
(+artem1, -artem2, ~artem3, !artem4)

val value = 0 :: 1 :: 2 :: 3 :: 4 :: Nil
value(3)
