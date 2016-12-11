import game._
import models.{Attack, Defend, GameSetup}

object Main {
  def main(args: Array[String]): Unit = {
    args match {
      case Array("attack", gameId, "x") => GameRunner.startGame(GameSetup(Attack, gameId, 1, 'x'))
      case Array("attack", gameId, "o") => GameRunner.startGame(GameSetup(Attack, gameId, 1, 'o'))
      case Array("defend", gameId, "x") => GameRunner.startGame(GameSetup(Defend, gameId, 2, 'x'))
      case Array("defend", gameId, "o") => GameRunner.startGame(GameSetup(Defend, gameId, 2, 'o'))
      case _ => println("USAGE: gameMode (attack or defend), gameId, playerSymbol (x or o)")
    }
  }
}