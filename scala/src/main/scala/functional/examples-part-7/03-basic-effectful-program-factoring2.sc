case class Player(name: String, score: Int)

// pure function, decide who is the winner
def winner(p1: Player, p2: Player): Option[Player] = // we will use None to indicate a draw
  if (p1.score > p2.score) Some(p1) // player 1 won
  else if (p2.score > p1.score) Some(p2) // player 2 won
  else None // it's a draw


// pure functions, decides what the winning messgage is
def winnerMsg(w: Option[Player]): String =
  w.map( p => s"${p.name} is the winner")
  .getOrElse("It's a draw. :(")


// just a single effectful statement of println, the other parts are pure
// and the effect is in the outermost layer of the computation
def contest(p1: Player, p2: Player): Unit =
  println(winnerMsg(winner(p1, p2)))

contest(
  Player("Paul", 6),
  Player("Bill", 4)
)
