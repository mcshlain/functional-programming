package functional.part6

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

import functional.part6.printUtils.detailedPrintln

object ab_basicFixedThreadPoolExample {

  case class FixedThreadPool(size: Int) {

    val tasksQueue = new mutable.Queue[() => Unit]()

    val threads = (0 until size).map{i =>
      val thread = new Thread(() => {
        while(true) {
          // try aquiring a task from the queue, if there isn't one, go to sleep
          var taskToDo: () => Unit = () => {}
          tasksQueue.synchronized {
            while(tasksQueue.isEmpty) {
              tasksQueue.wait() // go to sleep until somone wakes you up
            }
            taskToDo = tasksQueue.dequeue()
          }

          // run the task in a try catch block to avoid the task crashing the entire thread
          try {
            taskToDo()  // run the task
          } catch {
            // This will not catch Errors such as StackOverflowError, which we don't handle here
            case e: Exception => detailedPrintln(s"Task crashed with ${e.getMessage}")
          }
        }
      }, s"my-pool-thread-$i") // can give names to threads

      thread.start() // start the thread or it wont run

      thread
    }

    // we can use the scala lazy argument syntax to provide a slightly nicer user experiance
    def submit(task: => Unit): Unit = {
      tasksQueue.synchronized {
        tasksQueue.enqueue(() => task)
        tasksQueue.notify()
      }
    }

    def forceShutdown(): Unit = {
      // interupt will cause an Error to be thrown, effectivaely shutting down all threads in the middle of what ever
      // they were doing
      this.threads.foreach(_.interrupt())
    }

  }

  def main(args: Array[String]): Unit = {

    val threadPool = new FixedThreadPool(2);

    threadPool.submit {
      throw new RuntimeException("Oops!")
    }

    threadPool.submit {
      detailedPrintln("Starting task 1")
      Thread.sleep(2000)
      detailedPrintln("Finished task 1")
    }

    threadPool.submit {
      detailedPrintln("Starting task 2")
      Thread.sleep(2000)
      detailedPrintln("Finished task 2")
    }

    threadPool.submit {
      detailedPrintln("Starting task 3.1")
      Thread.sleep(2000)
      detailedPrintln("Finished task 3.1")

      // the next task that continues some computation
      threadPool.submit{
        detailedPrintln("Starting task 3.2")
        Thread.sleep(2000)
        detailedPrintln("Finished task 3.2")

        threadPool.forceShutdown()
      }
    }

  }

}
