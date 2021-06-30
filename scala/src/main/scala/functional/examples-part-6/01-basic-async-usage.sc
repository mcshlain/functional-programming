import scala.collection.mutable
import functional.part6.af_basicTask.*

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

// make this exceution available for the compier to fill in
given ExecutionContextExecutorService with
  ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))


val task1 = Async {
  println("Starting task1")
  println(Thread.sleep(2000))
  println("Finished task1")
}

task1.run



