import functional.part3.monad.*
import functional.part3.applicative.*
import functional.part3.monoid.*
import functional.part4.writerMonad.*

import scala.collection.mutable

type Logger[A] = Writer[List[String], A]
def log(s: String): Logger[Unit] = Writer.tell(List(s))


val five: Logger[Int] = Writer.value(5)
val fiveWiithLog: Logger[Int] = five << log("five!")

// example we've seen before
def pow2WithLog(a: Double): Logger[Double] = for (
  _ <- log(s"before power2 of $a");
  r = math.pow(a, 2);
  _ <- log(s"after power2 of $a with result of $r")
) yield r

pow2WithLog(4)

// instead of keepting track of logs lets keep track of number of executions
type ExecCounter[A] = Writer[Map[String, Int], A]
def count1(key: String): ExecCounter[Unit] = Writer.tell(Map(key -> 1))


def fibonacci(n: Int): ExecCounter[Int] = {
  if (n >= 2) {
    for (
      nm1 <- fibonacci(n - 1) << count1(s"fib(${n-1})");
      nm2 <- fibonacci(n - 2) << count1(s"fib(${n-2})")
    ) yield nm1 + nm2
  } else {
    Writer.value(1)
  }
}

fibonacci(4)
