// 1
def values(fun: Int => Int, low: Int, high: Int): IndexedSeq[(Int, Int)] = {
  for (i <- low to high) yield (i, fun(i))
}
values(x => x * x, -5, 5)

// 2
Array(4, 1, 43, 5, 1, 231, 53, 243, 543, 545, 36, 45, 6).reduceLeft(
  (a, b) => if (a > b) a else b
)

3
def factorial(x: Long): Long = {
  if (x <= 0) 1L
  else (1L to x).product // same as //(1 to n).reduceLeft(_ * _)
}

factorial(-1)
factorial(11)

// 4
def factorial2(n: Long): Long = {
  (1L to n).foldLeft(1L)(_ * _)
}
factorial2(-1)
factorial2(14)

// 5
def largest(fun: Int => Int, inputs: Seq[Int]): Int = {
  inputs.map(fun).max
}

largest(x => 10 * x - x * x, 1 to 10)

// 6
def largestAt(fun: Int => Int, inputs: Seq[Int]): Seq[Int] = {
  inputs.groupBy(fun)(largest(fun, inputs))
}
largestAt(x => 10 * x - x * x, 1 to 10)

// 7
def adjustToPair(fun: (Int, Int) => Int): ((Int, Int)) => Int = {
  pair: (Int, Int) =>
    fun(pair._1, pair._2)
}
def adjustToPairCurrying(fun: (Int, Int) => Int)(pair: (Int, Int)): Int = {
  fun(pair._1, pair._2)
}
adjustToPair(_ * _)((6, 7))
adjustToPairCurrying(_ * _)(6, 7)

def mapPairs(pairs: Seq[(Int, Int)], fun: (Int, Int) => Int): Seq[Int] = {
  pairs.map(adjustToPair(fun))
}

mapPairs(Seq((1, 2), (3, 4), (5, 6)), _ + _)

// 8
Array("no ban", "artem ban", "perma ban").corresponds(Array(6, 9, 9))(
  _.length == _
)

// 9
class CorrespondsMan(val xs: Array[String]) {
  def corresponds[B](that: Seq[B], p: (String, B) => Boolean): Boolean = {
    xs.corresponds(that)(p)
  }
}
//new CorrespondsMan(Array("no ban", "artem ban", "perma ban"))
//  .corresponds(Array(6, 9, 9), _.length == _)
// missing parameter type for expanded function

// 10
def callByValueUnless(condition: Boolean)(block: => Unit): Unit = {
  if (!condition) block
}
def callByNameUnless(condition: => Boolean)(block: => Unit): Unit = {
  if (!condition) block
}
def notCurryingUnless(condition: Boolean, block: => Unit): Unit = {
  if (!condition) block
}

var lessThan5Count1 = 0
for (i <- 1 to 7) {
  callByNameUnless(i >= 5) {
    lessThan5Count1 += 1
  }
}

lessThan5Count1 // should be 4

var lessThan5Count2 = 0
for (i <- 1 to 7) {
  // Syntax is a bit uglier but otherwise call-by-value works in this case just as well as call-by-name.
  // This is because the condition function value is evaluated only once per "unless" block evaluation.
  callByValueUnless(i >= 5) {
    lessThan5Count2 += 1
  }
}

lessThan5Count2 // should be 4

var lessThan5Count3 = 0
for (i <- 1 to 7) {
  // Note that the invocation syntax is different when "unless" does not utilize currying.
  notCurryingUnless(i >= 5, {
    lessThan5Count3 += 1
  })
}
lessThan5Count3 // should be 4