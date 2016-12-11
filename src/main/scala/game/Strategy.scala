package game

import models.Types.Moves
import models.Move

import scala.util.Random

object Strategy {
  def firstMove(playerSymbol: Char): Move = {
    Move(1, 1, playerSymbol)
  }

  def nextMove(playerSymbol: Char, moves: Moves): Option[Move] = {
    List(
      matchCriteria(nToWin(1), playerSymbol, moves),
      matchCriteria(oneToDefend, playerSymbol, moves),
      matchCriteria(nToWin(2), playerSymbol, moves),
      randomMove(playerSymbol, moves)
    ).flatten.headOption
  }

  def winnerExists(moves: Moves): Boolean = {
    val threeInALine = (movesGen: (Int, Char) => Moves, i: Int) => {
      (movesGen(i, 'x').intersect(moves).length == 3) || (movesGen(i, 'o').intersect(moves).length == 3)
    }

    val inARow = List(0, 1, 2).map(row => threeInALine(allRowMoves, row))
    val inAColumn = List(0, 1, 2).map(column => threeInALine(allColumnMoves, column))
    val inADiagonal = List(0, 1).map(diagonal => threeInALine(allDiagonalMoves, diagonal))

    (inARow ::: inAColumn ::: inADiagonal).reduce(_ || _)
  }

  def allRowMoves(rowN: Int, playerSymbol: Char): Moves = {
    for (x <- List(rowN); y <- List(0, 1, 2); v <- List(playerSymbol)) yield Move(x, y, v)
  }

  def allColumnMoves(columnN: Int, playerSymbol: Char): Moves = {
    for (x <- List(0, 1, 2); y <- List(columnN); v <- List(playerSymbol)) yield Move(x, y, v)
  }

  def allDiagonalMoves(diagonal: Int, playerSymbol: Char): Moves = diagonal match {
    case 0 => List(Move(0, 0, playerSymbol), Move(1, 1, playerSymbol), Move(2, 2, playerSymbol))
    case 1 => List(Move(0, 2, playerSymbol), Move(1, 1, playerSymbol), Move(2, 0, playerSymbol))
  }

  def allCornerMoves(playerSymbol: Char): Moves = {
    for (x <- List(0, 2); y <- List(0, 2); v <- List(playerSymbol)) yield Move(x, y, v)
  }

  private def opponent(symbol: Char): Char = symbol match {
    case 'x' => 'o'
    case 'o' => 'x'
  }

  private def inverseMovePlayer(move: Move): Move = {
    Move(move.x, move.y, opponent(move.v))
  }

  private def oneToDefend(possibleOurMoves: Moves, possibleOpponentMoves: Moves, moves: Moves): Option[Move] = {
    val ourMoves = possibleOurMoves.intersect(moves)
    val opponentMoves = possibleOpponentMoves.intersect(moves)

    if (ourMoves.isEmpty && opponentMoves.length == 2)
      return Some(inverseMovePlayer(possibleOpponentMoves.diff(opponentMoves).head))

    None
  }

  private def nToWin(left: Int)(possibleOurMoves: Moves, possibleOpponentMoves: Moves, moves: Moves): Option[Move] = {
    val ourMoves = possibleOurMoves.intersect(moves)
    val opponentMoves = possibleOpponentMoves.intersect(moves)

    if (ourMoves.length == (3 - left) && opponentMoves.isEmpty)
      return Some(possibleOurMoves.diff(ourMoves).head)

    None
  }

  private def checkByCriteria(criteria: (Moves, Moves, Moves) => Option[Move], playerSymbol: Char, moves: Moves,
                              allPossibleMovesGen: (Int, Char) => Moves, index: Int): Option[Move] = {
    val possibleOurMoves = allPossibleMovesGen(index, playerSymbol)
    val possibleOpponentMoves = allPossibleMovesGen(index, opponent(playerSymbol))

    criteria(possibleOurMoves, possibleOpponentMoves, moves)
  }

  private def checkRow(criteria: (Moves, Moves, Moves) => Option[Move], playerSymbol: Char, moves: Moves): List[Option[Move]] = {
    List(0, 1, 2).map(row => checkByCriteria(criteria, playerSymbol, moves, allRowMoves, row))
  }

  private def checkColumn(criteria: (Moves, Moves, Moves) => Option[Move], playerSymbol: Char, moves: Moves): List[Option[Move]] = {
    List(0, 1, 2).map(column => checkByCriteria(criteria, playerSymbol, moves, allColumnMoves, column))
  }

  private def checkDiagonal(criteria: (Moves, Moves, Moves) => Option[Move], playerSymbol: Char, moves: Moves): List[Option[Move]] = {
    List(0, 1).map(diagonal => checkByCriteria(criteria, playerSymbol, moves, allDiagonalMoves, diagonal))
  }

  private def matchCriteria(criteria: (Moves, Moves, Moves) => Option[Move], playerSymbol: Char, moves: Moves): Option[Move] = {
    val inARow = checkRow(criteria, playerSymbol, moves)
    val inAColumn = checkColumn(criteria, playerSymbol, moves)
    val inADiagonal = checkDiagonal(criteria, playerSymbol, moves)

    (inARow ::: inAColumn ::: inADiagonal).flatten.headOption
  }

  private def randomMove(playerSymbol: Char, moves: Moves): Option[Move] = {
    if(moves.nonEmpty) {
      val availableMoves = for (x <- List(0, 1, 2);
                                y <- List(0, 1, 2);
                                v <- List(playerSymbol)
                                if !moves.contains(Move(x, y, v)) && !moves.contains(inverseMovePlayer(Move(x, y, v)))
      ) yield Move(x, y, v)

      val centerMove = availableMoves.intersect(List(firstMove(playerSymbol)))
      val cornerMoves = availableMoves.intersect(allCornerMoves(playerSymbol))

      val movesToSelectFrom = List(centerMove, cornerMoves, availableMoves)
        .foldLeft(List[Move]())((a, b) => if (a.nonEmpty) a else b)

      if (movesToSelectFrom.nonEmpty) {
        val maxIndex = movesToSelectFrom.length - 1

        if (maxIndex > 0) {
          return Some(movesToSelectFrom(Random.nextInt(maxIndex)))
        } else {
          return Some(movesToSelectFrom(maxIndex))
        }
      }
    }

    None
  }
}