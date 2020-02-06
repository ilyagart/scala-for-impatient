// 1
final class ImmutablePair[T, S](val first: T, val second: S) {
  def swap(): ImmutablePair[S, T] = new ImmutablePair(second, first)
}

val immutablePair1 = new ImmutablePair(1, 2)
immutablePair1.first
immutablePair1.swap().first

// 2
class MutablePair[T](var comp1: T, var comp2: T) {
  def swap() {
    val buff = comp1
    comp1 = comp2
    comp2 = buff
  }
}
val mut1 = new MutablePair(1, 2)
mut1.comp1
mut1.comp2
mut1.swap()
mut1.comp1
mut1.comp2

// 3
def genericSwap[T, S](pair: ImmutablePair[T, S]): ImmutablePair[S, T] =
  new ImmutablePair[S, T](pair.second, pair.first)

val genericPair1 = new ImmutablePair("123", 321)
genericPair1.first
genericSwap(genericPair1).first

// 4
/*
Why don’t we need a lower bound for the replaceFirst method in Section 18.3, “Bounds
for Type Variables,” on page 266 if we want to replace the first component of a
Pair[Person] with a Student?

Student is sub-class for Person therefore it inherits all properties required
 for replaceFirst method
   */

// 5
/*
Why does RichInt implement Comparable[Int] and not Comparable[RichInt]?

To be able to use view bounds, like
 {{{
   class Pair[T <% Comparable[T]](val first: T, val second: T)
 }}}
 which than can be used with `Int` types, like
 {{{
   new Pair(1, 2)
 }}}
 we need to have implicit conversion from `T` to `Comparable[T]`. `RichInt` class
 implements `Comparable[Int]` and there is implicit conversion from `Int` to `RichInt`.
 So, we don't use `RichInt` class directly.
   */

// 6
/*
Write a generic method middle that returns the middle element from any Iterable[T].
For example, middle("World") is 'r'.
   */
def middle[T](xs: Iterable[T]): Option[T] = {
  // def middle[A, C](xs: C)(implicit ev: C <:< Iterable[A]): Option[A] = {
  // def middle[A, C <% Iterable[A]](xs: C): Option[A] = {
  // def middle[A, C](xs: C)(implicit ev: C => Iterable[A]): Option[A] = {
  val seq = xs.toSeq
  val length = seq.length
  if (length % 2 == 0) None
  else Option(seq(length / 2))
}
middle("World")
middle("banana")
middle(List(1, 2, 3))
middle(Array(true, false))

// 7
/*
Look through the methods of the Iterable[+A] trait. Which methods use the type parameter
A? Why is it in a covariant position in these methods?
TODO
   */

// 8
/*
In Section 18.10, “Co- and Contravariant Positions,” on page 272, the replaceFirst
method has a type bound. Why can’t you define an equivalent method on a mutable Pair[T]?
def replaceFirst[R >: T](newFirst: R) { first = newFirst } // Error
TODO
   */

// 9
/*
It may seem strange to restrict method parameters in an immutable class Pair[+T].
However, suppose you could define
def replaceFirst(newFirst: T)
in a Pair[+T]. The problem is that this method can be overridden in an unsound way.
Construct an example of the problem. Define a subclass NastyDoublePair of
Pair[Double] that overrides replaceFirst so that it makes a pair with the square root
of newFirst. Then construct the call replaceFirst("Hello") on a Pair[Any] that
is actually a NastyDoublePair.
TODO
   */

// 10
/*
Given a mutable Pair[S, T] class, use a type constraint to define a swap method that can
be called if the type parameters are the same.
   */
class MutablePairST[S, T](var first: S, var second: T)

def swapMutablePair[S, T](pair: MutablePairST[S, T])(implicit ev: T =:= S) {
  val tmp = pair.first.asInstanceOf[T]
  pair.first = pair.second
  pair.second = tmp
}

val mutablePairST1 = new MutablePairST(123, 456)
val mutablePairST2 = new MutablePairST("123", 456)
val mutablePairST3 = new MutablePairST(true, 123)
val mutablePairST4 = new MutablePairST("artem", "ban")
mutablePairST1.first
swapMutablePair(mutablePairST1)
mutablePairST1.first
mutablePairST2.first
//swapMutablePair(mutablePairST2) Error:(128, 16) Cannot prove that Int =:= String.
mutablePairST2.first
mutablePairST3.first
//swapMutablePair(mutablePairST3) Error:(131, 16) Cannot prove that Int =:= Boolean.
mutablePairST3.first
mutablePairST4.first
swapMutablePair(mutablePairST4)
mutablePairST4.first
