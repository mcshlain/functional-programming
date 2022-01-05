package functional.part8

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part4.stateMonad.*
import functional.part6.al_TaskAsMonadFixed.*
import functional.part8.ae_freeMonadWithFoldMap.*
import functional.part8.af_LazyMonad.*
import functional.part8.ad_naturalTransformation.*

import java.util.UUID
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.collection.mutable

object ag_userLoyaltyFreeInterpreters {

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
    } yield user.get


    // We can define a natural transformation from our free syntax to Lazy
    val inMemoryLazyInterpreter: UserRepositoryAlg ~> Lazy = new (UserRepositoryAlg ~> Lazy) {

      val memoryStorage = mutable.Map.empty[UUID, User]

      override def apply[A](fa: UserRepositoryAlg[A]): Lazy[A] = fa match {
        case FindUser(userId) => Lazy{
          () => memoryStorage.get(userId)
        }
        case UpdateUser(user) => Lazy{
          () => {
            memoryStorage.put(user.id, user)
            ()
          }
        }
      }
    }

    val programAsLazy = foldMap(program, inMemoryLazyInterpreter)

    println("Executing the program as the Lazy Monad: ")
    println(programAsLazy.eval())


    // We define a natural transformation from our syntax to Task, If our interpreter is Async it's better to
    // define as Task and not Lazy, because we'll need to make the Lazy interpreter blocking
    val inMemoryTaskInterpreter: UserRepositoryAlg ~> Task = new (UserRepositoryAlg ~> Task) {

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
    val programAsTask = foldMap(program, inMemoryTaskInterpreter)

    // We execute the program as a task
    given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

    println("Executing the program as the Task Monad (a.k.a Promise/Future): ")
    println(executeAsyncAndWait(programAsTask))

    ec.shutdown()


    // A more pure implementation is to transform our program is into a State Monad

    // We define a natural transformation from our syntax to Task, If our interpreter is Async it's better to
    // define as Task and not Lazy, because we'll need to make the Lazy interpreter blocking

    // State has two parameters so we define an alias to make it simpler to use
    type RepoState[A] = State[Map[UUID, User], A]

    val inMemoryStateInterpreter: UserRepositoryAlg ~> RepoState = new (UserRepositoryAlg ~> RepoState) {

      override def apply[A](fa: UserRepositoryAlg[A]): RepoState[A] = fa match {
        case FindUser(userId) => State { s0 =>
          (s0, s0.get(userId))
        }

        case UpdateUser(user) => State { s0 =>
          val s1 = s0 + (user.id -> user)
          (s1, ())
        }
      }
    }

    // We run the interpreter using foldMap
    val programAsState = foldMap(program, inMemoryStateInterpreter)

    println("Executing the program as the State Monad: ")
    val initialState = Map.empty[UUID, User]
    print(programAsState.run(initialState))

  }

}