import scala.collection.mutable.ArrayBuffer

// 1
class Bug() {
  private var pos = 0
  private var direction = 1
  def show(): this.type = { println(s"$pos"); this }
  def turn(): this.type = { direction *= -1; this }
  def move(len: Int): this.type = { pos += direction * len; this }
}

val bugsy = new Bug()
bugsy.move(4).show().move(6).show().turn().move(5).show()

// 2
/*
Provide a fluent interface for the Bug class of the preceding exercise, so that one can write
Click here to view code image
bugsy move 4 and show and then move 6 and show turn around move 5 and show
   */
object Show

object Then

object Around
val show = Show
val `then` = Then
val around = Around

trait SuperBug {
  this: Bug =>
  def and(obj: Show.type): this.type = this.show()
  def and(obj: Then.type): this.type = this
  def turn(obj: Around.type): this.type = this.turn()
}

val bugsy2_0 = new Bug with SuperBug

bugsy2_0 move 4 and show and then move 6 and show turn around move 5 and show

// 3
object Title

object Author

class Document(var title: String, var author: String) {
  private var useNextArgAs: Any = _
  def set(obj: Title.type): this.type = { useNextArgAs = obj; this }
  def set(obj: Author.type): this.type = { useNextArgAs = obj; this }
  def to(arg: String): this.type = useNextArgAs match {
    case Title  => title = arg; this
    case Author => author = arg; this
    case _      => this
  }
}

class Book(title: String, author: String) extends Document(title, author)
val book4 = new Book("Harry Potter", "Dan Balan")
book4.title
book4.author
book4 set Title to "Scala for the Impatient" set Author to "Cay Horstmann"
book4.title
book4.author

// 4
class Network {
  class Member(val name: String) {
    val contacts = new ArrayBuffer[Network#Member]

    override def equals(that: Any): Boolean = that match {
      case _: Member => true
      case _         => false
    }
    override def toString = s"Member($contacts, $name)"
  }

  private val members = new ArrayBuffer[Member]
  def join(name: String): Member = {
    val m = new Member(name)
    members += m
    m
  }
}
val chatter = new Network
val myFace = new Network
val fred = chatter.join("Fred") // Has type chatter.Member
val john = chatter.join("John") // Has type chatter.Member
val barney = myFace.join("Barney") // Has type myFace.Member
val alex = myFace.join("Alex") // Has type myFace.Member

fred equals alex // false
fred equals john // true
barney equals john // false
barney equals alex // true

// 5
/*
Consider the type alias
type NetworkMember = n.Member forSome { val n: Network }

and the function

def process(m1: NetworkMember, m2: NetworkMember) = (m1, m2)

How does this differ from the process function in Section 19.8, “Existential Types,” on page
286?

def process[M <: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2)

type NetworkMember means members from any network (M >: n.Member) while other M <: n.Member means strictly
members from the same network
   */

// 6
import scala.math._

def indexOrClosest(xs: Array[Int], i: Int): Int Either Int = {
  if (xs contains i) Left(i)
  else Right(xs.reduceLeft((a, b) => if (abs(a - i) < abs(b - i)) a else b))
}
val ints = Array(1, 2, 3, 10)
indexOrClosest(ints, 7) // 10
indexOrClosest(ints, 3) // 3

// 7
def processAndClose[T <: { def close(): Unit }, R](obj: T)(f: T => R): R = {
  try f(obj)
  finally obj.close()
}

val obj = new Object {
  var processed = false
  var closed = false

  def process(): Unit = {
    processed = true
  }

  def close(): Unit = {
    closed = true
  }
}

processAndClose(obj)(_.process())

obj.processed // true
obj.closed // true

//processAndClose(obj)(_ => throw new Exception()) //throw an exception

// 8
/*
Write a function printValues with three parameters f, from, to that prints all values of f
with inputs from the given range. Here, f should be any object with an apply method that
consumes and yields an Int
   */
def printValues(f: { def apply(x: Int): Int }, from: Int, to: Int) {
  for (i <- from to to) print(f(i) + " ")
}
printValues(Array(1, 1, 2, 3, 5, 8, 13, 21, 34, 55), 3, 6)
val square = (x: Int) => x * x

square apply 4
//printValues(square, 3, 6) NoSuchMethodException scala compiler bug

// 9
abstract class Dim[T](val value: Double, val name: String) { this: T =>
  protected def create(v: Double): T
  def +(other: Dim[T]) = create(value + other.value)
  override def toString = s"$value $name"
}

class Seconds(v: Double) extends Dim[Seconds](v, "s") {
  override def create(v: Double) = new Seconds(v)
}

class Meters(v: Double) extends Dim[Meters](v, "m") {
  override def create(v: Double) = new Meters(v)
}
// allowing meters and seconds to be added. Use a self type to prevent that.

val seconds1 = new Seconds(1.0)
val seconds2 = new Seconds(2.0)
val meters1 = new Meters(3.0)
val meters2 = new Meters(4.0)
(seconds1 + seconds2).toString // "3.0 s"
(meters1 + meters2).toString // "7.0 m"
//(seconds1 + meters2).toString Error

// 10
/*
Self types can usually be replaced with traits that extend classes, but there can be situations
where using self types changes the initialization and override orders. Construct such an
example.
   */
class A(val name: String) {

  override def toString: String = name
}

trait Named { self: A =>

  override val name: String = "Named: " + self.name

  override def toString: String = "from Named: " + super.toString
}

val someObj = new A("someObj") with Named
someObj.toString // from Named: Named: null
someObj.name