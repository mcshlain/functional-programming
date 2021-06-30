package functional.part6

import functional.part3.applicative.*
import functional.part3.functor.*
import functional.part6.printUtils.detailedPrintln

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object aj_TaskAsApplicativeParallel {

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

  // Define how Task is an Applicative Functor
  given Applicative[Task] with {

    def pure[A](a: A): Task[A] = Task {
      a
    }

    override def ap[A, B](fa: Task[A], ff: Task[A => B]): Task[B] = Task {
      (callbackB: Callback[B], ec: ExecutionContext) => {
        val lock = new Object()
        var resF: Option[A => B] = None
        var resA: Option[A] = None

        ff.executeAsync((fr: AsyncResult[A => B]) => fr match {
          case Success(f) =>
            var resB: Option[B] = None
            lock.synchronized {
              resF = Some(f)
              resA.foreach{ a =>
                resB = Some(f(a))
              }
            }
            resB.foreach(b => callbackB(Success(b)))
          case Failure(e) => callbackB(Failure(e))
        })(using ec)

        fa.executeAsync((fa: AsyncResult[A]) => fa match {
          case Success(a) =>
            var resB: Option[B] = None
            lock.synchronized {
              resA = Some(a)
              resF.foreach{ f =>
                resB = Some(f(a))
              }
            }
            resB.foreach(b => callbackB(Success(b)))
          case Failure(e) => callbackB(Failure(e))
        })(using ec)
      }
    }
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

    given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

    def operation[A](r: A) = Task {
      detailedPrintln(s"Starting operation to compute $r")
      Thread.sleep(2000)
      detailedPrintln(s"Finished operatio to compute $r")

      r // result of the task
    }

    val operation1 = Applicative[Task].product(operation(7).map(_ + 3), operation(9))
    val resultOp1 = executeAsyncAndWait(operation1);
    detailedPrintln(s"Waited and task finished resulting in '${resultOp1}'")


    val operations = List(operation(1), operation(2), operation(3), operation(4))
    val operation2 = Applicative[Task].sequence(operations)
    val resultOp2 = executeAsyncAndWait(operation2);
    detailedPrintln(s"Waited and task finished resulting in '${resultOp2}'")


    ec.shutdown()
  }
}
