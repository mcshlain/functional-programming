package functional.part8

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part6.al_TaskAsMonadFixed.*
import functional.part8.ae_freeMonadWithFoldMap.*
import functional.part8.ad_naturalTransformation.*

import java.util.UUID
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.collection.mutable

object af_userLoyaltyFreeInterpreter {

  case class User(id: UUID, email: String, loyaltyPoints: Int)


  // Step 1
  // ------
  // Define the basic operations of our DSL (as data)
  // NOTE: We avoid committing to a specific effect so we don't have any mention of 'Future' here
  sealed trait UserRepositoryAlg[A]
  case class FindUser(userId: UUID) extends UserRepositoryAlg[Option[User]]
  case class UpdateUser(user: User) extends UserRepositoryAlg[Unit]


  // Step 2
  // ------
  // Lets create an alias type for our freed syntax. it's always annoying to work with types that have
  // multiple parameters
  // NOTE: UserRepository is a language where UserRepositoryAlg is the instruction set

  type UserRepository[A] = FreeMonad[UserRepositoryAlg, A]


  // Step 3
  // ------
  // Create constructors for our operations that will encapsulate the lifting of our operations into the FreeMonad
  // Otherwise we will need to deal with explicit casting to help the compiler with its inference
  // NOTE: we essentially take individual instructions and lift them into being a tiny program which only does that
  //       one instruction (we want that because the program can be sequenced, while the instructions by them
  //       selves can't they are just case classes, they don't have the monad semantics)

  def findUser(userId: UUID): UserRepository[Option[User]] = liftFM(FindUser(userId))
  def updateUser(user: User): UserRepository[Unit] = liftFM(UpdateUser(user))


  // Step 4
  // ------
  // Write our business logic using the new language we defined

  type Error = String

  def addPoints(userId: UUID, pointsToAdd: Int): UserRepository[Either[Error, Unit]] = {
    findUser(userId).flatMap{
      case None => Monad[UserRepository].pure(Left("No user found"))
      case Some(user) =>
        val updatedUser = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
        updateUser(updatedUser).map(_ => Right(()))
    }
  }


  def main(args: Array[String]): Unit = {

    // since the free monad is a monad we can write programs with it
    val userId = UUID.randomUUID()
    val program = for {
      _ <- updateUser(User(userId, "mail", 0))
      _ <- addPoints(userId, 6)
      user <- findUser(userId)
    } yield user


    // We define a natural transformation from our syntax to Task, and this is essentially our interpreter
    // Very simply we just specify for every instruction the equivalent Task
    val inMemoryInterpreter: UserRepositoryAlg ~> Task = new NaturalTransformation[UserRepositoryAlg, Task] {

      val memoryStorage = mutable.Map.empty[UUID, User]

      override def apply[A](fa: UserRepositoryAlg[A]): Task[A] = fa match {
        case FindUser(userId) => Task{
          memoryStorage.get(userId)
        }
        case UpdateUser(user) => Task{
          memoryStorage.put(user.id, user)
          ()
        }
      }
    }

    // We run the interpreter using foldMap
    val programAsTask = foldMap(program, inMemoryInterpreter)

    // We execute the program as a task
    given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

    val user = executeAsyncAndWait(programAsTask)
    print(user)

    ec.shutdown()
  }

}