import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}
//import scala.util.control.TailCalls.{TailRec, done, tailcall}

object Main extends App {
//  import scala.collection.parallel.CollectionConverters.RangeIsParallelizable
//  for (i <- (0 until 100).par) print(s" $i")
//  println((for (i <- (0 until 100000).par) yield i) == (0 until 100000))
//  def evenLength(xs: Seq[Int]): TailRec[Boolean] =
//    if (xs.isEmpty) done(true) else tailcall(oddLength(xs.tail))
//  def oddLength(xs: Seq[Int]): TailRec[Boolean] =
//    if (xs.isEmpty) done(false) else tailcall(evenLength(xs.tail))
//
//  println(oddLength(Seq(1, 2, 3)).result)
  import java.time._
  import scala.concurrent._
  import ExecutionContext.Implicits.global
//  val f3 = Future {
//    Thread.sleep(10000)
//    if (scala.util.Random.nextInt < 0.5) throw new Exception
//    42
//  }
//  f3.onComplete {
//    case Success(v)  => println(s"The answer is $v")
//    case Failure(ex) => println(ex.getMessage)
//  }

  trait Event[S] {
    var source: S = _
  }

  trait Listener[S, E <: Event[S]] {
    def occurred(e: E): Unit
  }

  trait Source[S, E <: Event[S], L <: Listener[S, E]] {
    this: S =>
    private val listeners = new ArrayBuffer[L]
    def add(l: L) { listeners += l }
    def remove(l: L) { listeners -= l }
    def fire(e: E) {
      e.source = this // Self type needed here
      for (l <- listeners) l.occurred(e)
    }
  }

  class ButtonEvent extends Event[Button]

  trait ButtonListener extends Listener[Button, ButtonEvent]

  class Button extends Source[Button, ButtonEvent, ButtonListener] {
    def click() { fire(new ButtonEvent) }
  }

  val button = new Button
  button.click()
}
