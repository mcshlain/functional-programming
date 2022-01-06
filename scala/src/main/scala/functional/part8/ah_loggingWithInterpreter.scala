package functional.part8

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part4.stateMonad.*
import functional.part6.al_TaskAsMonadFixed.*
import functional.part8.ae_freeMonadWithFoldMap.*
import functional.part8.af_LazyMonad.*
import functional.part8.ad_naturalTransformation.*


import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object ah_loggingWithInterpreter {

  enum LoggingLevel(ord: Int, repr: String) {
    case ERROR   extends LoggingLevel(4, "ERROR")
    case WARNING extends LoggingLevel(3, "WARN ") // lazy way for consistent string size
    case INFO    extends LoggingLevel(2, "INFO ") // lazy way for consistent string size
    case DEBUG   extends LoggingLevel(1, "DEBUG")
    case TRACE   extends LoggingLevel(0, "TRACE")

    override def toString: String = repr

    def orderIndex: Int = ord
  }

  // Step 1
  // ------
  // Define the basic operations of our DSL (as data)
  // We should probably add some basic operations to set the log level but lets keep this simple
  sealed trait LoggingInstructionSet[A]
  case class LogMessage(level: LoggingLevel, msg: String) extends LoggingInstructionSet[Unit]
  case class SetLogLevel(level: LoggingLevel) extends LoggingInstructionSet[Unit]
  case object GetLogLevel extends LoggingInstructionSet[LoggingLevel]


  // Step 2
  // ------
  // Lets create an alias type for our freed syntax. it's always annoying to work with types that have
  // multiple parameters

  type Logging[A] = FreeMonad[LoggingInstructionSet, A]


  // Step 3
  // ------
  // Create constructors for our operations that will encapsulate the lifting of our operations into the FreeMonad
  def logInfo(msg: => String): Logging[Unit] = liftFM(LogMessage(LoggingLevel.INFO, msg))
  def logDebug(msg: => String): Logging[Unit] = liftFM(LogMessage(LoggingLevel.DEBUG, msg))
  def logWarning(msg: => String): Logging[Unit] = liftFM(LogMessage(LoggingLevel.WARNING, msg))
  def logError(msg: => String): Logging[Unit] = liftFM(LogMessage(LoggingLevel.ERROR, msg))
  def logTrace(msg: => String): Logging[Unit] = liftFM(LogMessage(LoggingLevel.TRACE, msg))

  def setLogLevel(level: LoggingLevel): Logging[Unit] = liftFM(SetLogLevel(level))
  def getLogLevel: Logging[LoggingLevel] = liftFM(GetLogLevel)


  // Lazy / sync
  val loggingLazyInterpreter: LoggingInstructionSet ~> Lazy = new (LoggingInstructionSet ~> Lazy) {

    private var logLevel = LoggingLevel.TRACE

    override def apply[A](fa: LoggingInstructionSet[A]): Lazy[A] = fa match {
      case LogMessage(level, msg) => Lazy(() => {
        if (logLevel.orderIndex <= level.orderIndex) {
          println(s"[$level] $msg")
        }
      })
      case SetLogLevel(level) => Lazy(() => {
        logLevel = level
      })
      case GetLogLevel => Lazy(() => logLevel)
    }
  }

  // Task / async
  val loggingTaskInterpreter: LoggingInstructionSet ~> Task = new (LoggingInstructionSet ~> Task) {

    private var logLevel = LoggingLevel.TRACE

    override def apply[A](fa: LoggingInstructionSet[A]): Task[A] = fa match {
      case LogMessage(level, msg) => Task{
        if (logLevel.orderIndex <= level.orderIndex) {
          println(s"[$level] $msg")
        }
      }
      case SetLogLevel(level) => Task{
        logLevel = level
      }
      case GetLogLevel => Task(logLevel)
    }
  }

  // Pure state
  type LoggingState[A] = State[LoggingLevel, A]

  val loggingStateInterpreter: LoggingInstructionSet ~> LoggingState = new (LoggingInstructionSet ~> LoggingState) {
    override def apply[A](fa: LoggingInstructionSet[A]): LoggingState[A] = fa match {
      case LogMessage(level, msg) => State {
        currentLevel => {
          if (currentLevel.orderIndex <= level.orderIndex) {
            println(s"[$level] $msg")
          }
          (currentLevel, ()) // the state is unchanged
        }
      }
      case SetLogLevel(newLevel) => State(_ => (newLevel, ()))
      case GetLogLevel => State(currentLevel => (currentLevel, currentLevel))
    }
  }

  def main(args: Array[String]): Unit = {
    // Not a very interesting program that just does logging
    val logAllLevel = for {
      _ <- logError("This is an error log")
      _ <- logWarning("This is an warning log")
      _ <- logInfo("This is an info log")
      _ <- logDebug("This is an debug log")
      _ <- logTrace("This is an trace log")
    } yield ()

    val program = for {
      _ <- logAllLevel
      _ <- setLogLevel(LoggingLevel.WARNING)
      _ <- logAllLevel
    } yield ()

    val programAsLazy = foldMap(program, loggingLazyInterpreter)

    println("Executing the program as the Lazy Monad: ")
    println(programAsLazy.eval())

    // We run the interpreter using foldMap
    val programAsTask = foldMap(program, loggingTaskInterpreter)

    // We execute the program as a task
    given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

    println("Executing the program as the Task Monad (a.k.a Promise/Future): ")
    println(executeAsyncAndWait(programAsTask))

    ec.shutdown()

    // We run the interpreter using foldMap
    val programAsState = foldMap(program, loggingStateInterpreter)

    println("Executing the program as the State Monad: ")
    print(programAsState.run(LoggingLevel.ERROR))
  }

}
