import functional.part3.monoid.*

case class Player(name: String, score: Int)

// Definition of a simple IO type and creating a "description' for the println operation
trait IO {
  def run: Unit  // this function is the "interpreter" of the IO "description"
}

def PrintLine(msg: String): IO =
  new IO {
    def run = println(msg)
  }


given Monoid[IO] with {
  override def empty = new IO{
    def run = ()  // an effectful description that does nothing
  }

  override def combine(a: IO, b: IO) = new IO {  // combine to effects to run one after the other
    def run = {
      a.run
      b.run
    }
  }
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
val contestDescription = contest(
  Player("Paul", 6),
  Player("Bill", 4)
)

val listOfPrograms: List[IO] = List (
  PrintLine("Hello Paul"),
  PrintLine("Hello Bill"),
  contestDescription
)

// combine All is defined on Lists that have elements that form a Monoid
listOfPrograms.combineAll.run