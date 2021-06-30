package functional.part6

import functional.part6.printUtils.detailedPrintln

import java.util.concurrent.Executors
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object ag_basicAwait {

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


  // For a more complete implementation we also need to add variations with timeout
  def executeAsyncAndWait[A](a: Task[A])(using ec: ExecutionContext): A = {
    val lock = new Object();
    var result: Option[AsyncResult[A]] = None;

    a.executeAsync((r: AsyncResult[A]) => {
      lock.synchronized {
        result = Some(r)
        lock.notify() // in case we're blocking/sleeping on the lock, awake
      }
    })

    lock.synchronized {
      if (!result.isDefined) {
        lock.wait()
      }
    }

    val v = result.getOrElse(throw new AssertionError("Something terrible went wrong"))
    v match {
      case Success(r) => r
      case Failure(e) => throw e
    }
  }

  def main(args: Array[String]): Unit = {

    given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))

    def operation(str: String): Task[String] = Task {
      detailedPrintln("Starting operation")
      Thread.sleep(2000)
      detailedPrintln("Finished operation")

      str // result of the task
    }

    val resultTask1 = executeAsyncAndWait(operation("result1"));
    detailedPrintln(s"Waited and task 1 finished resulting in '${resultTask1}'")


    val task2 = Task {
      val resultTask1 = executeAsyncAndWait(operation("subresult1"));
      detailedPrintln(s"Waited and task 2.1 finished resulting in '${resultTask1}'")

      val resultTask2 = executeAsyncAndWait(operation("subresult1"));
      detailedPrintln(s"Waited and task 2.2 finished resulting in '${resultTask2}'")

      (resultTask1, resultTask2)
    }

    val resultTask2 = executeAsyncAndWait(task2);
    detailedPrintln(s"Waited and task 2 finished resulting in '${resultTask2}'")

    ec.shutdown()

  }

}
