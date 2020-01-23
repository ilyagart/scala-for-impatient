import java.io.IOException

import scala.annotation.{elidable, switch, tailrec, varargs}
import scala.beans.BeanProperty
import scala.collection.immutable.HashMap

@volatile var isDone = false
@transient var recentLookups = new HashMap[String, String]
// Becomes a transient field in the JVM

class Book {
  @throws(classOf[IOException]) def read(filename: String): Unit = {}
}

//The Java signature is
//void read(String filename) throws IOException
@varargs def process(args: String*) = args.mkString(",")
//translates into void process(String... args) // Java bridge method

class Person {
  @BeanProperty var name: String = _
}
/*
generates methods
getName() : String
setName(newValue : String) : Unit
   */

def sum(xs: Seq[Int]): BigInt =
  if (xs.isEmpty) 0 else xs.head + sum(xs.tail)

@tailrec
def sum2(xs: Seq[Int], partial: BigInt): BigInt =
  if (xs.isEmpty) partial else sum2(xs.tail, xs.head + partial)

// sum(1 to 1000000) Stack overflow error
sum2(1 to 1000000, 0)

val n = 1

(n: @switch) match {
  case 0 => "Zero"
  case 1 => "One"
  case _ => "?"
}
@elidable(scala.annotation.elidable.FINE) def dump(props: Map[String, String]) { }
