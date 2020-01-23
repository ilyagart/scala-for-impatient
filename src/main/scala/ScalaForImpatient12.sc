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
/*
Implement an unless control abstraction that works just like if, but with an inverted
condition. Does the first parameter need to be a call-by-name parameter? Do you need
currying?
   */
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

lessThan5Count1

var lessThan5Count2 = 0
for (i <- 1 to 7) {
  // Syntax is a bit uglier but otherwise call-by-value works in this case just as well as call-by-name.
  // This is because the condition function value is evaluated only once per "unless" block evaluation.
  callByValueUnless(i >= 5) {
    lessThan5Count2 += 1
  }
}

lessThan5Count2

var lessThan5Count3 = 0
for (i <- 1 to 7) {
  // Note that the invocation syntax is different when "unless" does not utilize currying.
  notCurryingUnless(i >= 5, {
    lessThan5Count3 += 1
  })
}
lessThan5Count3

2.toInt
45L.toBinaryString
val vladik: Int => String = _.toString.concat(" -molodec")
val vova: Int => String = _.toString.concat(" - vonu4ii gomosek")
vladik(69)
vova(1)