// Chapter 3. Working with Arrays
import scala.collection.mutable.ArrayBuffer

val ints = Array(1, 2, 3, 4, 5)
for (elem <- ints if elem % 2 == 0) yield elem
ints.filter(_ % 2 == 0)
val b = ArrayBuffer[Int]()
b += 1
b :+ (1, 2, 3, 5)
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

