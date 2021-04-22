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
    var count = 0;
    computeAnyGame(O, 4) foreach {g => printBoardsToConsole(g); count+=1; println()}
    //... X.. X.. X.. XO.
    //... ... O.. O.. O..
    //... ... ... X.. X..
    //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
    //
    //... ... .O. XO. XOO
    //... ... ... ... ...
    //... .X. .X. .X. .X.

    assertEquals(3024,count)
  }
}
