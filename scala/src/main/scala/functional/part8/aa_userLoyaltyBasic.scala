package functional.part8

import java.util.UUID
import functional.part6.al_TaskAsMonadFixed.*
import functional.part3.monad.*
import functional.part3.applicative.*


object aa_userLoyaltyBasic {


  case class User(id: UUID, email: String, loyaltyPoints: Int)


  // An interface of some sort of a low level storage mechanism for out users
  // There could be different implementations for this interface
  // Maybe we store users in a data base, maybe it's in files ....
  trait UserRepository {
    def findUser(userId: UUID): Task[Option[User]]
    def updateUser(user: User): Task[Unit]
  }


  // A business logic service with operations on users
  type Error = String


  // dependencies are provided through the constructor
  class LoyaltyService(usersRepo: UserRepository) {

    def addPoints(userId: UUID, pointsToAdd: Int): Task[Either[Error, Unit]] = {
      usersRepo.findUser(userId).flatMap{
        case None => Task(Left("No user found"))
        case Some(user) =>
          val updatedUser = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
          usersRepo.updateUser(updatedUser).map(_ => Right(()))
      }
    }
  }

  // NOTE: notice how in our business layer we are stuck with Task, just because the lower level language in
  // user repository is using tasks

}