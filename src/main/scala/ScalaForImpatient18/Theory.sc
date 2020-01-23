import java.net.URL

class Pair[T, S](val first: T, val second: S)

new Pair("1", 2)
val p2 = new Pair[Any, Any](42, "String")
def getMiddle[T](a: Array[T]): T = a(a.length / 2)
getMiddle(Array(1, 2, 3, 4, 5, 6, 7))
val f = getMiddle[String] _

class Pair[T](val first: T, val second: T)

class ComparablePair[T <: Comparable[T]](val first: T, val second: T) {
  def smaller =
    if (first.compareTo(second) < 0) first else second
  // no error because it's upper bounded by Comparable[T]
}

val url = new URL("https://www.scala-exercises.org/")
//new ComparablePair(url, url).smaller won't Error:(20, 1) inferred type arguments [java.net.URL] do not conform to value <local ComparablePair>'s type parameter bounds [T <: Comparable[T]]
new ComparablePair("lol", "kek").smaller

class ReplacePair[T](val first: T, val second: T) {
  def replaceFirst[R >: T](newFirst: R) = new ReplacePair[R](newFirst, second)
}
new ReplacePair("Kek", "Vasea").replaceFirst("Molodec")

class RichPair[T <% Comparable[T]] //deprecated

class OrderingPair[T: Ordering](val first: T, val second: T) {
  def smaller(implicit ord: Ordering[T]): T =
    if (ord.compare(first, second) < 0) first else second
}

new OrderingPair(1, 2).smaller

import scala.reflect._

def makePair[T: ClassTag](first: T, second: T) = {
  val r = new Array[T](2); r(0) = first; r(1) = second; r
}

makePair[Int](4, 5)

/*
If Student is a subclass of Person, can I call makeFriend with a Pair[Student]? By
default, this is an error. Even though Student is a subtype of Person, there is no relationship
between Pair[Student] and Pair[Person].
If you want such a relationship, you have to indicate it when you define the Pair class:
   */
class CovariantPair[+T](val first: T, val second: T)

/*
The + means that the type is covariant in T—that is, it varies in the same direction. Since Student
is a subtype of Person, a Pair[Student] is now a subtype of Pair[Person].
   */
trait Friend[-T] {
  def befriend(someone: T)
}

def makeFriendWith(s: Student, f: Friend[Student]) { f.befriend(s) }

class Person extends Friend[Person] {
  def befriend(someone: Person): Unit = ???
}

class Student extends Person
val susan = new Student
val fred = new Person

/*
will the call makeFriendWith(susan, fred) succeed? It seems like it should. If Fred is
willing to befriend any person, he’ll surely like to be friends with Susan.
Note that the type varies in the opposite direction of the subtype relationship. Student is a subtype
of Person, but Friend[Student] is a supertype of Friend[Person]. In that case, you
   */
class AnotherPair[+T](val first: T, val second: T) {
  //def replaceFirst(newFirst: T) = new AnotherPair[T](newFirst, second) // Error
  // The compiler rejects this, because the parameter type T is in a contravariant position. Yet this method
  //cannot damage the pair—it returns a new pair.
  def replaceFirst[R >: T](newFirst: R) = new AnotherPair[R](newFirst, second)
}

class InvariantPair[T](var first: T, var second: T)

def makeFriends(p: InvariantPair[_ <: Person]) //can use wildcard with upper bound inside method because class is invariant

import java.util.Comparator

def min[T](p: InvariantPair[T])(comp: Comparator[_ >: T])
