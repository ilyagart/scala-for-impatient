import java.awt.datatransfer.{DataFlavor, SystemFlavorMap}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

val ints = Array(1, 2, 3, 4, 5)
for (elem <- ints if elem % 2 == 0) yield elem
ints.filter(_ % 2 == 0)
val b = ArrayBuffer[Int]()
b += 1
//b += (1, 2, 3, 5)
b ++= Array(8, 13, 21)
//b trimEnd 5
b

for (i <- b.indices)
  println(s"$i: ${b(i)}")

for (elem <- b)
  println(elem)

val a = ArrayBuffer(2, 3, -5, 5, 7, 11, -2, -4)
val result = for (elem <- a if elem % 2 == 0) yield 2 * elem
a.filter(_ % 2 == 0).map(2 * _)
a filter (_ % 2 == 0) map (2 * _)

def cleanNegs(x: ArrayBuffer[Int]) {
  val positionsToKeep = for (i <- x.indices if x(i) >= 0) yield i
  for (j <- positionsToKeep.indices) x(j) = x(positionsToKeep(j))
  x.trimEnd(x.length - positionsToKeep.length)
}

cleanNegs(a)
a

Array(1, 7, 2, 9).sum

ArrayBuffer("Mary", "had", "a", "little", "lamb").max

val h = ArrayBuffer(1, 7, 2, 9)
val hSorted = h.sortBy(_ % 2 == 0)
val hDescending = h.sortWith(_ < _)

a.mkString(" and ")
a.mkString("<", ",", ">")

val matrix = Array.ofDim[Double](3, 4)
matrix(0)(0) = 42
val triangle = new Array[Array[Int]](10)
for (i <- triangle.indices)
  triangle(i) = new Array[Int](i + 1)

val kek = Array("Mary", "a", "had", "lamb", "little")
java.util.Arrays.binarySearch(kek.asInstanceOf[Array[Object]], "little")
val result = kek.search("little")

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

val command = ArrayBuffer("ls", "-al", "/home/cay")
val pb = BufferHasAsJava(command) // Scala to Java
pb.asJava

//Exercises
def setRandomValuesToArray(a: Array[Int], n: Int) {
  for (i <- a.indices) {
    a(i) = Random.nextInt(n)
  }
}
val kraken = Array(1, 2, 3, 4, 5, 6)
setRandomValuesToArray(kraken, 25)
kraken

a
var buf = 0
for (i <- a.indices if i % 2 != 0) {
  buf = a(i)
  a(i) = a(i - 1)
  a(i - 1) = buf
}
a

val r = Array[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
for (i <- r.indices)
  yield
    if (r(i) == r.last) r(i)
    else {
      if (i % 2 == 0) r(i + 1)
      else r(i - 1)
    }

val posNegArray = Array[Int](1, 0, -1, 2, -2, 3, 4, 5, 45, 123, 4, 0, 364,
  -234, -243324, 0, -132123)
val sortedArray = posNegArray.filter(_ > 0) ++ posNegArray.filter(_ <= 0)

val doubleArray = Array[Double](1, 2, 3, 3.0, 2.2, 6.6)
doubleArray.foldLeft(0.0) {
  _ + _
} / doubleArray.length
doubleArray.sum / doubleArray.length

val reverseSortedArray = Array(1, 5, 32, 4, 2, 23, 5).sorted.reverse
val reverseSortedArrayBuffer =
  ArrayBuffer(1, 5, 32, 4, 2, 23, 5).sorted.reverse

reverseSortedArray.sortWith(_ > _)
reverseSortedArrayBuffer.sortWith(_ > _)

posNegArray.distinct

val inefficient = posNegArray.toBuffer
var first = true
var n = inefficient.length
var i = 0
while (i < n) {
  if (inefficient(i) >= 0) i += 1
  else {
    if (first) {
      first = false
      i += 1
    } else {
      inefficient.remove(i)
      n -= 1
    }
  }
}
inefficient

val efficient = posNegArray.toBuffer

var negativeIndices = for (i <- efficient.indices if efficient(i) < 0) yield i
negativeIndices = negativeIndices drop 1
for (i <- negativeIndices.reverse) efficient.remove(i)
efficient

posNegArray
for (i <- posNegArray.indices
     if i == posNegArray.indexWhere(_ < 0) || posNegArray(i) >= 0)
  yield posNegArray(i)

val timeZones = java.util.TimeZone.getAvailableIDs

var filteredZones = timeZones
  .filter(_.contains("America"))
  .map(_.stripPrefix("America/"))
  .sorted

val flavors =
  SystemFlavorMap.getDefaultFlavorMap.asInstanceOf[SystemFlavorMap]
val natives = flavors.getNativesForFlavor(DataFlavor.imageFlavor)
val vals = scala.jdk.CollectionConverters
  .CollectionHasAsScala(new java.util.LinkedList(natives))
vals.asScala