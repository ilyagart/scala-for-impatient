class Document2(var title: String, var author: String) {
  def setTitle(title: String): this.type = { this.title = title; this }
  def setAuthor(author: String) = { this.author = author; this }
}
val article = new Document2("1", "2")
article.setTitle("Whatever Floats Your Boat").setAuthor("Cay Horstmann")

class Book2(title: String, author: String, var chapter: String)
    extends Document2(title, author) {
  def addChapter(chapter: String) = { this.chapter = chapter; this }
}

val book2 = new Book2(article.title, article.author, "2")
book2.setTitle("Scala for the Impatient") //can't do .addChapter() here
book2.addChapter("chapter1")
book2.setTitle("Test").addChapter("chapter2") //possible because of
// def setTitle(title: String): this.type
object Title

class Document(var title: String) {
  private var useNextArgAs: Any = _
  def set(obj: Title.type): this.type = { useNextArgAs = obj; this }
  def to(arg: String): Unit =
    if (useNextArgAs == Title) title = arg; else println("")
}

class Book(title: String) extends Document(title)
val book3 = new Book("Harry Potter")
book3.title
book3 set Title to "Scala for the Impatient" // parsed as book3.set(Title).to("Scala for the Impatient")
book3.title

import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class Network {
  class Member(val name: String) {
    val contacts = new ArrayBuffer[Network#Member]

    override def toString = s"Member($contacts, $name)"
  }
  private val members = new ArrayBuffer[Member]
  def join(name: String) = {
    val m = new Member(name)
    members += m
    m
  }
}
val chatter = new Network
val myFace = new Network
val fred = chatter.join("Fred") // Has type chatter.Member
val barney = myFace.join("Barney") // Has type myFace.Member
fred.contacts addOne barney //  no error because val contacts = new ArrayBuffer[Network#Member]
//means "a Member of any Network"
var chatter2 = new Network
//val fred2 = new chatter2.Member("some") impossible with chatter2 being var since its unstable

class Example1 {
  type Index = mutable.HashMap[String, (Int, Int)]
}

def appendLines(target: { def appendAll(xs: IterableOnce[String]): Any }, lines: Iterable[String]) {
  { target.appendAll(lines); target.appendAll(Array("\n")) }
}

val arrayBuffer = ArrayBuffer("1", "2")
appendLines(arrayBuffer, Array("artem", "ban"))
arrayBuffer

import scala.language.postfixOps
Map[String, Int]("asdf" -> 1, "gadg" -> 3)
def process[M <: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2)
val wilma = chatter.join("Wilma")
process(fred, wilma) // OK
//process(fred, barney) //  Error:(75, 1) inferred type arguments [Network#Member] do not conform to method process's type parameter bounds [M <: n.Member forSome { val n: Network }]
scala.language.existentials

def square(x: Int): Int = x * x
val squareV = (x: Int) => x * x
val squareL: Int => Int = x => x * x
val funcOfSquare = square _
def triple(x: Int): Int = 3 * x
val funcOfTriple = triple _
val tripleV = (x: Int) => 3 * x
val tripleL: Int => Int = (x: Int) => 3 * x
val x = 5
square(x)
squareV(x)
squareL(x)
funcOfSquare(x)
triple(x)
tripleV(x)
tripleL(x)
funcOfTriple(x)

trait Logged {
  def log(msg: String)
}

trait LoggedException extends Logged {
  this: Exception =>
  def log() { log(getMessage) }
  // OK to call getMessage because this is an Exception
}

trait Group {
  outer: Network => // not self type; members can refer to group as Member.outer
  class Member {}
}

trait Logger { def log(msg: String) }

trait ConsoleLogger extends Logger with Cloneable with Serializable {
  def log(msg: String) { println(msg) }
}

trait FileLogger extends Logger {
  val out = new PrintWriter("app.log") // Part of the trait's constructor
  out.println(s"# ${java.time.Instant.now()}") // Also part of the constructor
  def log(msg: String) { out.println(msg); out.flush() }
}

trait ListenerSupport {
  type S <: Source
  type E <: Event
  type L <: Listener
  trait Event {
    var source: S = _
  }
  trait Listener {
    def occurred(e: E): Unit
  }
  trait Source {
    this: S =>
    private val listeners = new ArrayBuffer[L]
    def add(l: L) { listeners += l }
    def remove(l: L) { listeners -= l }
    def fire(e: E) {
      e.source = this
      for (l <- listeners) l.occurred(e)
    }
  }
}

object ButtonModule extends ListenerSupport {
  type S = Button
  type E = ButtonEvent
  type L = ButtonListener
  class ButtonEvent extends Event
  trait ButtonListener extends Listener
  class Button extends Source {
    def click() { fire(new ButtonEvent) }
  }
}
val b = new ButtonModule.Button
b.add((e: ButtonModule.ButtonEvent) => { println(s"$e :D") })
b.click()
b.click()
b.click()

trait Container[E] {
  def +=(e: E): Unit
}

trait Iterable[E, C[_]] {
  def iterator(): Iterator[E]
  def build[F](): C[F]
  def map[F](f: E => F): C[F] = {
    var res = build[F]()
    val iter = iterator()
    while (iter.hasNext) res += f(iter.next())
    res
  }
}

class Range(val low: Int, val high: Int) extends Iterable[Int, Buffer] {
  def iterator() = new Iterator[Int] {
    private var i = low
    def hasNext = i <= high
    def next() = { i += 1; i - 1 }
  }
  def build[F]() = new Buffer[F]
}

class Buffer[E: ClassTag] extends Iterable[E, Buffer] with Container[E] {
  private var capacity = 10
  private var length = 0
  private var elems = new Array[E](capacity) // See note
  def iterator() = new Iterator[E] {
    private var i = 0
    def hasNext = i < length
    def next() = { i += 1; elems(i - 1) }
  }
  def build[F]() = new Buffer[F]
  def +=(e: E) {
    if (length == capacity) {
      capacity = 2 * capacity
      val nelems = new Array[E](capacity) // See note
      for (i <- 0 until length) nelems(i) = elems(i)
      elems = nelems
    }
    elems(length) = e
    length += 1
  }
}
