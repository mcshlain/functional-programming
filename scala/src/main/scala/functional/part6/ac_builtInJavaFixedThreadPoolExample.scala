package functional.part6

import java.util.concurrent.Executors;
import functional.part6.printUtils.detailedPrintln

object ac_builtInJavaFixedThreadPoolExample {

  val executorService = Executors.newFixedThreadPool(2)

  def main(args: Array[String]): Unit = {

    // we need to cast to Runnable because submit is overloaded with two version
    // 1. Runnable which is () => Unit
    // 2. Callable<T> which is () => T
    // Java was not designed for functional programming in this case, and it needs separate interface since
    // void doesn't behave like other types so you can't have Callable<void> unlike scalas Unit
    executorService.submit(() => {
      throw new RuntimeException("Oops!")
    }.asInstanceOf[Runnable])

    executorService.submit(() => {
      detailedPrintln("Starting task 1")
      Thread.sleep(2000)
      detailedPrintln("Finished task 1")
    }.asInstanceOf[Runnable])

    executorService.submit(() => {
      detailedPrintln("Starting task 2")
      Thread.sleep(2000)
      detailedPrintln("Finished task 2")
    }.asInstanceOf[Runnable])

    executorService.submit(() => {
      detailedPrintln("Starting task 3.1")
      Thread.sleep(2000)
      detailedPrintln("Finished task 3.1")

      // the next task that continues some computation
      executorService.submit(() => {
        detailedPrintln("Starting task 3.2")
        Thread.sleep(2000)
        detailedPrintln("Finished task 3.2")

        executorService.shutdown()
      }.asInstanceOf[Runnable])
    }.asInstanceOf[Runnable])

  }

}
