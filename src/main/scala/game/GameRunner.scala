package game

import models.Types.Moves
import models.{Attack, GameSetup}

import scala.collection.immutable.List

object GameRunner {
  def startGame(gameSetup: GameSetup): Unit = {
    if (gameSetup.gameMode == Attack)
      Network.sendMoves(gameSetup.gameId, gameSetup.playerId, List(Strategy.firstMove(gameSetup.playerSymbol)))
    makeRegularMoves(gameSetup.gameId, gameSetup.playerId, gameSetup.playerSymbol, true)
  }

  private def makeRegularMoves(gameId: String, playerId: Int, symbol: Char, continue: Boolean): Unit = {
    continue match {
      case true => makeNextMove(gameId, playerId, symbol, Network.getMoves(gameId, playerId))
      case false => println("Game over!")
    }
  }

  private def makeNextMove(gameId: String, playerId: Int, symbol: Char, maybeMoves: Option[Moves]): Unit = {
    maybeMoves match {
      case Some(moves) =>
        if (moves.length < 9 && !Strategy.winnerExists(moves)) {
          Strategy.nextMove(symbol, moves) match {
            case Some(move) =>
              Network.sendMoves(gameId, playerId, moves ::: List(move))
              makeRegularMoves(gameId, playerId, symbol, moves.length != 8)
            case None =>
              println("Do not know where to move!")
              makeRegularMoves(gameId, playerId, symbol, false)
          }
        } else {
          makeRegularMoves(gameId, playerId, symbol, false)
        }
      case None => makeRegularMoves(gameId, playerId, symbol, false)
    }
  }
}