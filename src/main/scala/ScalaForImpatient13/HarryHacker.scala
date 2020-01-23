package ScalaForImpatient13

import scala.collection.mutable

object HarryHacker extends App {
  // 11
  /*
    Harry Hacker reads a file into a string and wants to use a parallel collection to update the letter
frequencies concurrently on portions of the string. He uses the following code:
   */
  import scala.collection.parallel.CollectionConverters._
  val str =
    "Harry Hacker Harry Potter Volan de Mort Ron Weasley Hermione Granger".toList

//  val frequencies = new scala.collection.mutable.HashMap[Char, Int]
//  for (c <- str.par) frequencies(c) = frequencies.getOrElse(c, 0) + 1
//  println(frequencies)
  /*
  Why is this a terrible idea? How can he really parallelize the computation? (Hint: Use
aggregate.)

Its bad because the data gets mutated
   */
  println(
    str.par
      .aggregate(new mutable.HashMap[Char, Int])((freq, c) => {
        freq(c) = freq.getOrElse(c, 0) + 1
        freq
      }, (freq, freq2) => {
        for ((k, v) <- freq2) freq(k) = freq.getOrElse(k, 0) + v
        freq
      })
      .toMap
  )
}
