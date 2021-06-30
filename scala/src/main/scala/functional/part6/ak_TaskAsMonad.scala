package functional.part6

import functional.part3.monad.*
import functional.part6.printUtils.detailedPrintln

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object ak_TaskAsMonad {

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
  given Monad[Task] with {

    def pure[A](a: A): Task[A] = Task {
      a
    }

    def flatMap[A, B](ma: Task[A], f: A => Task[B]): Task[B] = Task (
      (callbackB: Callback[B], ec: ExecutionContext) => {
        ma.executeAsync((ra: AsyncResult[A]) => ra match {
          case Success(a) =>
            f(a).executeAsync((br: AsyncResult[B]) => br match {
              case Success(b) => callbackB(Success(b))
              case Failure(e) => callbackB(Failure(e))
            })(using ec)
          case Failure(e) => callbackB(Failure(e))
        })(using ec)
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

    given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

    // again with a monad we now have access to the for-yield syntax which is nice
    case class User(id: Int, name: String)
    case class Address(city: String, street: String)

    def getUserByName(name: String): Task[User] = Task {

      detailedPrintln(s"Starting getUserByName for $name")
      Thread.sleep(2000)
      detailedPrintln(s"Finished getUserByName for $name")

      if (name != "Jim") {
        throw new RuntimeException("User not found!")
      } else {
        User(1, "Jim")
      }
    }

    def getAddressByUserId(id: Int): Task[Address] = Task {
      detailedPrintln(s"Starting getAddressByUserId for id:$id")
      Thread.sleep(2000)
      detailedPrintln(s"Finished getAddressByUserId for id:$id")

      if (id != 1) {
        throw new RuntimeException("Address not found!")
      } else {
        Address("Tel-Aviv", "Albert Mendler")
      }
    }

    def getAddressOfUser(name: String) = for(
      user <- getUserByName(name);
      address <- getAddressByUserId(user.id)
    ) yield address


    val jimsAddress = executeAsyncAndWait(getAddressOfUser("Jim"));
    detailedPrintln(s"Jims address '${jimsAddress}'")


    try {
      val jacksAddress = executeAsyncAndWait(getAddressOfUser("Jack"));
      detailedPrintln(s"Jacks address '${jacksAddress}'")
    } catch {
      case e: RuntimeException =>
        detailedPrintln(s"Getting Jacks address failed")
    }

    def operation[A](r: A) = Task {
      detailedPrintln(s"Starting operation to compute $r")
      Thread.sleep(2000)
      detailedPrintln(s"Finished operatio to compute $r")

      r // result of the task
    }

    val operation1 = operation(7).map(_ + 3)
    val resultOp1 = executeAsyncAndWait(operation1);
    detailedPrintln(s"Waited and task finished resulting in '${resultOp1}'")


    val operations = List(operation(1), operation(2), operation(3), operation(4))
    val operation2 = Monad[Task].sequence(operations)
    val resultOp2 = executeAsyncAndWait(operation2);
    detailedPrintln(s"Waited and task finished resulting in '${resultOp2}'")


    ec.shutdown()
  }
}
