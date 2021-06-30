package functional.part6

import functional.part6.printUtils.detailedPrintln

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object ad_callbacks {

  // can wrap around a java executor
  val executionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))


  def main(args: Array[String]): Unit = {

    // again with a monad we now have access to the for-yield syntax which is nice
    case class User(id: Int, name: String)
    case class Address(city: String, street: String)

    def getUserByName(name: String, callback: User => Unit, errorCallback: Throwable => Unit): Unit = {

      detailedPrintln(s"Starting getUserByName for $name")
      Thread.sleep(2000)
      detailedPrintln(s"Finished getUserByName for $name")

      if (name == "Jim") {
        callback(User(1, "Jim"))
      } else {
        errorCallback(new RuntimeException("Can't find the user"))
      }
    }

    def getAddressByUserId(id: Int, callback: Address => Unit, errorCallback: Throwable => Unit): Unit = {
      detailedPrintln(s"Starting getAddressByUserId for id:$id")
      Thread.sleep(2000)
      detailedPrintln(s"Finished getAddressByUserId for id:$id")

      if (id != 1) {
        errorCallback(new RuntimeException("Address not found!"))
      } else {
        callback(Address("Tel-Aviv", "Albert Mendler"))
      }
    }

    def getUserAddressByName(name: String, callback: Address => Unit, errorCallback: Throwable => Unit): Unit = {
      getUserByName(
        "Jim",
        user => getAddressByUserId(user.id, callback, _ => detailedPrintln("Failed getting Address")),
        _ => detailedPrintln("Failed getting User")
      )
    }


    // we need to cast to Runnable because submit is overloaded with two version
    // 1. Runnable which is () => Unit
    // 2. Callable<T> which is () => T
    // Java was not designed for functional programming in this case, and it needs separate interface since
    // void doesn't behave like other types so you can't have Callable<void> unlike scalas Unit
    executionContext.execute(() => {
      getUserAddressByName(
        "Jim",
        address => {
          detailedPrintln(s"Got address $address")
          executionContext.shutdown()
        },
        error => {
          detailedPrintln("Got an error")
          executionContext.shutdown()
        }
      )
    })
  }

}
