package u06lab.solution

object TicTacToe {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board.collectFirst( { case Mark(`x`,`y`, p) => p } )

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    for (x <- 0 to 2; y <- 0 to 2; if find(board, x, y).isEmpty ) yield Mark(x, y, player) :: board
  }

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = ???

  def printBoards(game: Seq[Board])(p: String => Any): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      p(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { p(" "); if (board == game.head) p("\n")}
    }

  //implicit val printToConsole: String => Unit = print
  def printBoardsToConsole(game: Seq[Board]): Unit = printBoards(game)(print)
  def printBoardsToString(game: Seq[Board]): String = { var m:String = ""; printBoards(game)(m+=_); m }
}

import TicTacToe._
object TryTicTacToe extends App {
  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoardsToConsole(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  printBoardsToConsole(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4) foreach {g => printBoardsToConsole(g); println()}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}