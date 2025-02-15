package u06lab.solution

import org.junit.jupiter.api.{Assertions, Test}
import Assertions._
import TicTacToe._

class TicTacToeTest {

  @Test
  def findTest() = {
    assertEquals(Some(X), find(List(Mark(0,0,X)),0,0))
    assertEquals(Some(O), find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1))
    assertEquals(None, find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1))
  }

  @Test
  def placeAnyMarkTest() = {
    // Exercise 2: implement placeAnyMark such that..
    val output1:String = "" +
      "... ... ..X ... ... .X. ... ... X.. \n" +
      "... ..X ... ... .X. ... ... X.. ... \n" +
      "..X ... ... .X. ... ... X.. ... ... \n"
    printBoardsToConsole(placeAnyMark(List(),X))
    assertEquals(output1,printBoardsToString(placeAnyMark(List(),X)))

    println()
    val output2:String = "" +
      "O.. O.. O.X O.. O.. OX. O.. O.. \n" +
      "... ..X ... ... .X. ... ... X.. \n" +
      "..X ... ... .X. ... ... X.. ... \n"
    printBoardsToConsole(placeAnyMark(List(Mark(0,0,O)),X))
    assertEquals(output2,printBoardsToString(placeAnyMark(List(Mark(0,0,O)),X)))
  }

  @Test
  def computeAnyGameTest() = {
    // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
    //... X.. X.. X.. XO.
    //... ... O.. O.. O..
    //... ... ... X.. X..
    //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
    //
    //... ... .O. XO. XOO
    //... ... ... ... ...
    //... .X. .X. .X. .X.

    // PRINT TEST
    // var count = 0;
    // computeAnyGame(O, 4) foreach {g => printBoardsToConsole(g); count+=1; println()}
    // assertEquals(9*8*7*6,count)

    assertEquals(9*8*7*6,computeAnyGame(O, 4).size)
  }

  @Test
  def checkRows() = {
    assertTrue(checkBoard(List(Mark(0,0,X), Mark(0,1,X), Mark(0,2,X)))(row))
    assertTrue(checkBoard(List(Mark(1,0,X), Mark(1,1,X), Mark(1,2,X)))(row))
    assertTrue(checkBoard(List(Mark(0,0,O), Mark(0,1,O), Mark(0,2,O)))(row))

    assertFalse(checkBoard(List())(row))
    assertFalse(checkBoard(List(Mark(0,0,X), Mark(0,1,O), Mark(0,2,X)))(row))
    assertFalse(checkBoard(List(Mark(0,0,X), Mark(1,1,X), Mark(0,2,X)))(row))
  }

  @Test
  def checkColumn() = {
    assertTrue(checkBoard(List(Mark(0,0,X), Mark(1,0,X), Mark(2,0,X)))(column))
    assertTrue(checkBoard(List(Mark(0,1,X), Mark(1,1,X), Mark(2,1,X)))(column))
    assertTrue(checkBoard(List(Mark(0,0,O), Mark(1,0,O), Mark(2,0,O)))(column))

    assertFalse(checkBoard(List())(column))
    assertFalse(checkBoard(List(Mark(0,0,X), Mark(1,0,O), Mark(2,0,X)))(column))
    assertFalse(checkBoard(List(Mark(0,0,X), Mark(1,1,X), Mark(0,2,X)))(column))
  }

  @Test
  def checkDiagonal() = {
    assertTrue(checkBoard(List(Mark(0,0,X), Mark(1,1,X), Mark(2,2,X)))(diagonal))
    assertTrue(checkBoard(List(Mark(0,2,X), Mark(1,1,X), Mark(2,0,X)))(diagonal))
    assertTrue(checkBoard(List(Mark(0,0,O), Mark(1,1,O), Mark(2,2,O)))(diagonal))

    assertFalse(checkBoard(List())(column))
    assertFalse(checkBoard(List(Mark(0,0,X), Mark(1,1,O), Mark(2,2,X)))(diagonal))
    assertFalse(checkBoard(List(Mark(0,0,X), Mark(1,1,X), Mark(0,2,X)))(diagonal))
  }

  @Test
  def checkGameOver() = {
    assertTrue(gameOver(List(Mark(0,0,X), Mark(0,1,X), Mark(0,2,X))))
    assertTrue(gameOver(List(Mark(0,1,X), Mark(1,1,X), Mark(2,1,X))))
    assertTrue(gameOver(List(Mark(0,0,X), Mark(1,1,X), Mark(2,2,X))))

    assertFalse(gameOver(List()))
    assertFalse(gameOver(List(Mark(0,0,X), Mark(1,1,O), Mark(2,2,X))))
    assertFalse(gameOver(List(Mark(0,0,X), Mark(1,1,X), Mark(0,2,X))))
  }

  @Test
  def computeAnyGameOverTest() = {
    var won = 0;
    var draw = 0;
    computeAnyGameOver(O, 9) foreach {
      g => if (gameOver(g.head)) won+=1 else draw +=1
    }

    // http://www.se16.info/hgb/tictactoe.htm
    assertEquals(209088, won)
    assertEquals(46080, draw)
  }
}
