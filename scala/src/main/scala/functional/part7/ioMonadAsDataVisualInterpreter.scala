package functional.part7

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part3.monoid.*

import scala.annotation.tailrec
import scala.io.StdIn
import javax.swing.*
import java.awt.*
import java.awt.event.{KeyEvent, KeyListener}
import scala.collection.mutable
import scala.collection.mutable.Stack

object ioMonadAsDataVisualInterpreter {

  // IO Monad implemented as data
  sealed trait ConsoleIO[+A]

  // a pure computation the immediatly returns an A
  // For example: pure will be implemented using this data constructor
  case class Pure[A](a: A) extends ConsoleIO[A]

  // A suspension to the computation where resume is a function that takes not arguments
  // but has some effect and yields a result
  // For example: reading information from the user will be implemented using this data constructor
  case class Suspend[A](resume: () => A) extends ConsoleIO[A]

  // A composition of two steps, when the interpreter will encounter this it will first process
  // the 'sub' computation and then based on its result process the second computation (available through f)
  case class FlatMap[A, B](sub: ConsoleIO[A], f: A => ConsoleIO[B]) extends ConsoleIO[B]


  // define basic operations in terms of data, so that they can be interpreted in different ways
  // this is less opaque as compared to representing them in Suspend
  case class ReadLine() extends ConsoleIO[String]

  case class PrintLine(str: String) extends ConsoleIO[Unit]


  // The Monad definition becomes trivial, the operations are just the corresponding
  // data constructors
  given Monad[ConsoleIO] with {
    override def pure[A](a: A): ConsoleIO[A] = Pure(a)
    override def flatMap[A, B](ma: ConsoleIO[A], f: A => ConsoleIO[B]): ConsoleIO[B] = FlatMap(ma, f)
  }

  // A visual interpreter that uses local mutation to keep the computation away from the stack
  def visualRun[A](program: ConsoleIO[A]): Unit = {

    val blockingObject = new Object();

    val window = new JFrame(); //creating instance of JFrame
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val inputTextArea  = new JTextArea()
    inputTextArea.setEditable(false) // we will make editable when we evaluate the ReadLine command

    val outputTextArea  = new JTextArea()
    outputTextArea.setEditable(false)
    val outputAreaScrollPane = new JScrollPane(outputTextArea)

    val panel = new JPanel()
    panel.setLayout(new BorderLayout())
    panel.add(inputTextArea, BorderLayout.NORTH)
    panel.add(outputAreaScrollPane, BorderLayout.CENTER)

    window.add(panel)
    window.pack()
    window.setVisible(true)

    // the data structure we will use to represent our computation stack  (that will be located in the heap maemory)
    val stack: Stack[ConsoleIO[Any]] = new mutable.Stack[ConsoleIO[Any]]()  // computation stack
    var returnedValue: Any = () // the returned value from the last computation

    // push our progeam to the stack to evaluate it
    stack.push(program)

    while(stack.length > 0) {
      // we need to do some down casting here
      val topFrame: ConsoleIO[A] = stack.pop().asInstanceOf[ConsoleIO[A]]

      println(s"Interpreting $topFrame")

      topFrame match {
        case Pure(a) =>
          returnedValue = a
        case Suspend(f) =>
          returnedValue = f()
        case PrintLine(str) =>
          outputTextArea.append(str + "\n")
        case ReadLine() =>
          val keyListener: KeyListener = new KeyListener {
            override def keyTyped(keyEvent: KeyEvent): Unit = {}

            override def keyReleased(keyEvent: KeyEvent): Unit = {}

            override def keyPressed(keyEvent: KeyEvent): Unit =  {
              if( keyEvent.getKeyChar == '\n') {
                keyEvent.consume() // prevent default behaviour of adding a new line to the input

                // save the content of the input (it will be the return value)
                returnedValue = inputTextArea.getText()

                // unblock the intepreter thread
                blockingObject.synchronized {
                  blockingObject.notify()
                }
              }
            }
          }

          inputTextArea.addKeyListener(keyListener)
          inputTextArea.setEditable(true)
          inputTextArea.requestFocus()

          // need to block until enter is pressed
          blockingObject.synchronized {
            blockingObject.wait()
          }

          // clear input
          inputTextArea.setText("")

          // need to disable the input again
          inputTextArea.setEditable(false)
          inputTextArea.removeKeyListener(keyListener)

        case FlatMap(ma, f) =>
          // The meaning of flatMap
          // 1. run ma
          // 2. pass the returned value from ma to f to get the next comutation g
          // 3. run g

          // step 2 + 3
          val g = f.asInstanceOf[Any => ConsoleIO[Any]] // just to make this compile
          stack.push(Suspend(() => {
            val nextStep = g(returnedValue) // step 2
            stack.push(nextStep) // step 3
            () // return Unit, instead of the stack (which is the return type of push)
          }))

          // step 1
          stack.push(ma)
      }

    }

    // we finished the program, we can dispose of the window now
    window.dispose()
  }


  // More combinators
  // we made them specific to IO this time because only for IO we created this tail recursive interpreter
  // so for other monads these implementations are not stack safe

  def forever[A](ma: ConsoleIO[A]): ConsoleIO[Nothing] = {
    lazy val t = forever(ma)
    ma.flatMap(_ => t)
  }


  def doWhile[A](ma: ConsoleIO[A])(cond: A => ConsoleIO[Boolean]): ConsoleIO[Unit] = for {
    a <- ma
    keepGoing <- cond(a)
    _ <- if(keepGoing) doWhile(ma)(cond) else Monad[ConsoleIO].pure(())
  } yield ()


  def foldM[A, B](lst: LazyList[A])(zero: B)(f: (B, A) => ConsoleIO[B]): ConsoleIO[B] = {
    lst match {
      case h #:: t => f(zero, h).flatMap(z2 => foldM(t)(z2)(f))
      case _ => Monad[ConsoleIO].pure(zero)
    }
  }


  // Examples of programs that use the combinators
  val simpleProgram =
    for {
      _ <- PrintLine("Enter some text")
      txt <- ReadLine()
      _ <- PrintLine(s"The User entered: '$txt'")
    } yield ()

  val foreverExample = forever {
    for {
      _ <- PrintLine("Input something")
      input <- ReadLine()
      _ <- PrintLine(s"You entered: $input")
    } yield ()
  }

  val doWhileExample = for {
    _ <- PrintLine("Enter something, q for exit")
    _ <- doWhile(ReadLine()){ input =>
      if (input == "q") {
        PrintLine(s"Exiting based on user request").map(_ => false)
      } else {
        PrintLine(s"got $input").map(_ => true)
      }
    }
  } yield ()

  // Now this large fold will no longer crash!!!
  val foldMExample = foldM[Int, Int]((1 to 100000).to(LazyList))(0){
    (acc, v) => PrintLine(s"Accamulator so far: $acc").map(_ => acc + v)
  }


  def main(args: Array[String]): Unit = {
    val comp = for {
      _ <- PrintLine("What is your name?")
      name <- ReadLine()
      _ <- PrintLine(s"Hello $name")
    } yield ()

//  val comp = doWhileExample
//  val comp = foreverExample
//  val comp = foldMExample

    visualRun(comp)
  }

}