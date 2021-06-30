package functional.part6

import scala.collection.mutable.ArrayBuffer
import functional.part6.printUtils.detailedPrintln

object aa_basicThreadUsageExample {

  def main(args: Array[String]): Unit = {

    val threads = new ArrayBuffer[Thread]

    // create threads
    for (i <- 0 until 10) {

      // Thread get an instance of Runnable but since it only has one method
      // of type () => Unit we can just pass in a function
      threads.addOne(
        new Thread(() => { // creating a new thread is expansive!!!
          detailedPrintln(s"Starting thread $i")
          Thread.sleep(i * 1000)
          detailedPrintln(s"Finished with thread $i")
        })
      )
    }

    // start the threads (make them actually run)
    for (thread <- threads) {
      thread.setDaemon(true) // don't keep alive if main thread exists
      thread.start()
    }

    // make the current thread wait for the other threads to complete
    for (thread <- threads) {
      thread.join()
    }

    detailedPrintln(s"All done!")

  }

}
