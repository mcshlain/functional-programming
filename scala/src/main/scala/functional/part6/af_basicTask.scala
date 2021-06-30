package functional.part6

import java.util.concurrent.Executors
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import functional.part6.printUtils.detailedPrintln

object af_basicTask {


  // shorter syntax of scala 3 instead of using a sealed trait and inheritance
  enum AsyncResult[A] {
    case Success(result: A)
    case Failure(t: Throwable)
  }

  // by importing this, we can use Success and Failure directly without the AsyncResult prefix (AsyncResult.Success,...)

  import AsyncResult.*


  // As always when we add a more specific Either type, it's good to have a conversion to the more general type
  extension[A] (x: AsyncResult[A]) {

    def toEither: Either[Throwable, A] = x match {
      case Success(r) => Right(r)
      case Failure(t) => Left(t)
    }
  }

  type Callback[A] = AsyncResult[A] => Unit

  // syntactic sugar
  case class Task[A](
    run: (Callback[A], ExecutionContext) => Unit
  ) {

    // execute in the same thread
    def execute(callback: Callback[A])(using ec: ExecutionContext): Unit = {
      run(callback, ec)
    }

    // variation with no callback if we want to ignore the result
    def execute(using ec: ExecutionContext): Unit = execute((_) => {})

    // execute in a new thread
    def executeAsync(callback: Callback[A])(using ec: ExecutionContext): Unit = {
      ec.execute(() => run(callback, ec))
    }

    // variation with no callback if we want to ignore the result
    def executeAsync(using ec: ExecutionContext): Unit = executeAsync((_) => {})

  }

  object Task {
    def apply[A](body: => A): Task[A] = Task(
      (callback: Callback[A], _: ExecutionContext) => {

        try {
          val result = body
          callback(Success(result))
        } catch {
          case e: Throwable => callback(Failure(e))
        }
      }
    )
  }

  def main(args: Array[String]): Unit = {

    given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))

    val task1 = Task {
      detailedPrintln("Starting task 1")
      Thread.sleep(2000)
      detailedPrintln("Finished task 1")
    }

    val task2 = Task {
      detailedPrintln("Starting task 2")
      Thread.sleep(2000)
      detailedPrintln("Finished task 2")
    }

    val task3 = Task {
      detailedPrintln("Starting task 3.2")
      Thread.sleep(2000)
      detailedPrintln("Finished task 3.2")

      ec.shutdown()
    }

    val task4 = Task {
      detailedPrintln("Starting task 3.1")
      Thread.sleep(2000)
      detailedPrintln("Finished task 3.1")

      task3.executeAsync
    }

    task1.executeAsync
    task2.executeAsync
    task4.executeAsync

  }

}
