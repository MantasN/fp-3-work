package game

import models.Move
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Strategy$Test extends FunSuite {

  test("returns (1,1) as the first move") {
    Strategy.firstMove('x') should equal(Move(1, 1, 'x'))
  }

  test("returns false if there is no winner") {
    Strategy.winnerExists(
      List(Move(0, 0, 'x'), Move(2, 2, 'o'), Move(1, 1, 'x'), Move(2, 0, 'o'), Move(1, 2, 'x'))
    ) should equal(false)
  }

  test("returns true if winner exists") {
    Strategy.winnerExists(
      List(Move(0, 0, 'x'), Move(1, 0, 'x'), Move(2, 0, 'x'), Move(0, 2, 'o'), Move(2, 2, 'o'))
    ) should equal(true)
  }

  test("second move is in the one of the corners") {
    val nextMove = Strategy.nextMove('o',
      List(Move(1, 1, 'x'))
    ).get

    Strategy.allCornerMoves('o') should contain(nextMove)
  }

  test("puts last if only one left for us") {
    Strategy.nextMove('x',
      List(Move(1, 1, 'x'), Move(0, 1, 'o'), Move(0, 0, 'x'), Move(2, 1, 'o'))
    ).get should equal(Move(2, 2, 'x'))
  }

  test("try to defend if only one left for the opponent") {
    Strategy.nextMove('o',
      List(Move(1, 1, 'x'), Move(0, 1, 'o'), Move(0, 0, 'x'))
    ).get should equal(Move(2, 2, 'o'))
  }

  test("puts in the same line if there is a empty line") {
    val nextMove = Strategy.nextMove('o',
      List(Move(1, 1, 'x'), Move(0, 0, 'o'), Move(2, 2, 'x'))
    ).get

    List(Move(0, 1, 'o'), Move(0, 2, 'o'), Move(1, 0, 'o'), Move(2, 0, 'o')) should contain(nextMove)
  }

  test("puts randomly in the free space if nothing else matches") {
    val nextMove = Strategy.nextMove('o',
      List(Move(1, 1, 'x'), Move(2, 1, 'o'), Move(0, 1, 'x'), Move(0, 0, 'o'), Move(2, 0, 'x'), Move(0, 2, 'o'), Move(2, 2, 'x'))
    ).get

    List(Move(1, 0, 'o'), Move(1, 2, 'o')) should contain(nextMove)
  }

  test("returns None if board is full") {
    val nextMove = Strategy.nextMove('o',
      List(Move(1, 1, 'x'), Move(0, 1, 'o'), Move(0, 2, 'x'), Move(2, 0, 'o'), Move(1, 0, 'x'),
        Move(1, 2, 'o'), Move(2, 1, 'x'), Move(0, 0, 'o'), Move(2, 2, 'x'))
    )

    nextMove.isEmpty should equal(true)
  }

}