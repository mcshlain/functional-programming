case class Player(name: String, score: Int)

// pure function, decide who is the winner
def winner(p1: Player, p2: Player): Option[Player] = // we will use None to indicate a draw
  if (p1.score > p2.score) Some(p1) // player 1 won
  else if (p2.score > p1.score) Some(p2) // player 2 won
  else None // it's a draw

// effectful function, still has two responsibilities
// 1. computing which display message to use
// 2. displaying the message on the screen
def contest(p1: Player, p2: Player): Unit =
  winner(p1, p2) match {
    case Some(Player(name, _)) => println(s"$name is the winner")
    case None => println(s"It's a draw. :(")
  }

contest(
  Player("Paul", 6),
  Player("Bill", 4)
)
