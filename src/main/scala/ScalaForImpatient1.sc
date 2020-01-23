import scala.language.postfixOps
import scala.math.BigInt.probablePrime
import scala.math.{pow, sqrt}
import scala.util.Random
// 1
3.self

// 2
sqrt(3)
3 - pow(res1, 2)

//3 res are val

// 4
"crazy" * 3

// 5
10 max 2

// 6
BigInt(2) pow 1024

// 7
probablePrime(100, Random)

// 8
probablePrime(100, Random) toString 36

// 9
val s = "FirstLast"
s(0)
(s head)
s charAt 0
s.find(_.isValidChar)
s(s.length - 1)
s charAt (s.length - 1)
(s last)
s.findLast(_.isValidChar)

// 10
val string = "LittleMan"
string.take(6)
string.drop(6)
string.takeRight(5)
string.dropRight(6)
