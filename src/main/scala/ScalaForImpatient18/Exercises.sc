// 1
/*
Define an immutable class Pair[T, S] with a method swap that returns a new pair with the
components swapped.
   */
final class Pair[T, S](val first: T, val second: S) {
  def swap(): Pair[S, T] = new Pair(second, first)
}

val value = new Pair(1, 2)
value.first
value.swap().first