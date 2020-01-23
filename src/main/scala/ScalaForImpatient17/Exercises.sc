import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

// 1
/*Consider the expression
for (n1 <- Future { Thread.sleep(1000) ; 2 }
n2 <- Future { Thread.sleep(1000); 40 })
println(n1 + n2)
How is the expression translated to map and flatMap calls? Are the two futures executed
concurrently or one after the other? In which thread does the call to println occur?
f1 and then f2 gets executed
println occurs in f2's thread
   */
def f1 = Future {
  Thread.sleep(1000)
  2
}

def f2 = Future {
  Thread.sleep(1000)
  40
}
f1.flatMap(
  n1 =>
    f2.map(n2 => {
      println(n1 + n2)
      n1 + n2
    })
)

// 2
/*
Write a function doInOrder that, given two functions f: T => Future[U] and g: U
=> Future[V], produces a function T => Future[U] that, for a given t, eventually
yields g(f(t)).
   */
def doInOrder[T, U, V](f: T => Future[U],
                       g: U => Future[V]): T => Future[V] = { t: T =>
  f(t).flatMap(u => g(u))
}

// 3
def doInOrder2[T](fs: (T => Future[T])*): T => Future[T] = { t =>
  fs.foldLeft(Future.successful(t))((acc, f) => acc.flatMap(f))
}
def doInOrder3[T](fs: Seq[T => Future[T]]): T => Future[T] = {
  def evaluateInOrder[U](f: Future[U],
                         rem: Seq[U => Future[U]]): Future[U] = {
    if (rem.isEmpty) f
    else f.flatMap(u => evaluateInOrder(rem.head(u), rem.tail))
  }

  if (fs.isEmpty)
    throw new IllegalArgumentException("need at least one function")
  t: T =>
    evaluateInOrder(fs.head(t), fs.tail)
}

// 7
/*
Write a program that counts the prime numbers between 1 and n, as reported by
BigInt.isProbablePrime. Divide the interval into p parts, where p is the number of
available processors. Count the primes in each part in concurrent futures and combine the
results.
   */
private implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

def primeSeq(n: Int): Future[Int] = Future {
  (for (i <- 1 to n if BigInt(i).isProbablePrime(10)) yield 1).sum
}

/**
    * Count primes concurrently so that thread t evaluates integers (t, t+threads, t+2*threads, ...), thread t+1
    * evaluates integers (t+1, t+1+threads, t+1+2*threads, ...) etc.
    * <p>
    * Note that some threads may get to evaluate sequences like (2, 6, 10, ...) which are quite useless to evaluate.
    *
    * @param upTo last integer to evaluate
    * @return number of probable primes found
    */
def primesConcurrentTrivialSampling(upTo: Int): Future[Int] = {
  val processors = Runtime.getRuntime.availableProcessors()

  val partitionedComputations = (1 to processors)
    .map(
      partition =>
        Future {
          (partition to upTo by processors)
            .filter(BigInt(_).isProbablePrime(10))
            .count(_ => true)
      }
    )
  Future.sequence(partitionedComputations).map(s => s.sum)
}

/**
    * Count primes concurrently so that thread t evaluates integers (1, ..., upTo/threads), thread t+1 evaluates
    * integers (upTo/threads+1, ..., 2*upTo/threads) etc.
    *
    * @param upTo last integer to evaluate
    * @return number of probable primes found
    */
def primesConcurrentRanges(upTo: Int): Future[Int] = {
  val partitions = Runtime.getRuntime.availableProcessors()
  val partitionSize = (upTo.toDouble / partitions).ceil.toInt

  val partitionedComputations = (1 to partitions)
    .map(
      partition =>
        Future {
          (1 to partitionSize)
            .map(index => (partition - 1) * partitionSize + index)
            .takeWhile(_ <= upTo) // ensure the last partition does not exceed upTo
            .count(BigInt(_).isProbablePrime(10))
      }
    )
  Future.sequence(partitionedComputations).map(s => s.sum)
}
//primeSeq(30)
//primesConcurrentRanges(30)
//primesConcurrentTrivialSampling(30)

val performanceTestN = 5000000
val performanceTestExpectedPrimes = 348513

primeSeq(70) map { count =>
  count // shouldEqual 19
}

primeSeq(performanceTestN) map { count =>
  {
    (f"counting $count primes sequentially took")
    count // shouldEqual performanceTestExpectedPrimes
  }
}

primesConcurrentTrivialSampling(70) map { size =>
  size //shouldEqual 19
}

primesConcurrentTrivialSampling(performanceTestN) map { count =>
  {

    (f"counting $count primes with primesConcurrentTrivialSampling took ")
    count // shouldEqual performanceTestExpectedPrimes
  }
}

primesConcurrentRanges(performanceTestN) map { count =>
  {

    (f"counting $count primes with primesConcurrentRanges took")
    count // shouldEqual performanceTestExpectedPrimes
  }
}
