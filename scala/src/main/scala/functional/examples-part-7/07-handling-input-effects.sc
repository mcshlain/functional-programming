import functional.part3.monoid.*

import scala.io.StdIn

// Definition of a simple IO type and creating a "description' for the println operation
trait IO {
  def run: Unit  // this function is the "interpreter" of the IO "description"
}

def PrintLine(msg: String): IO =
  new IO {
    def run = println(msg)
  }


// pure function
def fahrenheitToCelsius(f: Double): Double =
  (f - 32) * 5.0/9.0

// impure function we want to factor out the IO out of
def converter(): Unit = {
  println("Enter a temperature in degrees Fahrenheit:")
  val d = StdIn.readLine.toDouble
  println(fahrenheitToCelsius(d))
}


// So how would be make the converter pure?

def pureConverter(): IO = {
  val prompt = PrintLine("Enter a temperature in degrees Fahrenheit:")
  ??? // Now what?
}
