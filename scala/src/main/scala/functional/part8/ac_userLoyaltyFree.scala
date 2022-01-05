package functional.part8

import java.util.UUID
import functional.part8.ab_freeMonad.*
import functional.part3.monad.*
import functional.part3.applicative.*


object ac_userLoyaltyFree {


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

    // we can look at the data structure created that represents our program
    print(program)

    // But how do we interpret this program now?
  }

}