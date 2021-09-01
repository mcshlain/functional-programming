case class Player(name: String, score: Int)

// Definition of a simple IO type and creating a "description' for the println operation
trait IO {
  def run: Unit  // this function is the "interpreter" of the IO "description"
}

def PrintLine(msg: String): IO =
  new IO {
    def run = println(msg)
  }

// pure function, decide who is the winner
def winner(p1: Player, p2: Player): Option[Player] = // we will use None to indicate a draw
  if (p1.score > p2.score) Some(p1) // player 1 won
  else if (p2.score > p1.score) Some(p2) // player 2 won
  else None // it's a draw


// pure functions, decides what the winning messgage is
def winnerMsg(w: Option[Player]): String =
  w.map( p => s"${p.name} is the winner")
  .getOrElse("It's a draw. :(")


// This function is now pure. it returns a description of type IO
def contest(p1: Player, p2: Player): IO =
  PrintLine(winnerMsg(winner(p1, p2)))

// this is just evaluation of pure functions, it doesn't "affect" anything
val contestIODescription = contest(
  Player("Paul", 6),
  Player("Bill", 4)
)


// we can now run the interpreter to "execute" the "description" we created, this is the only impure part of our program
contestIODescription.run


