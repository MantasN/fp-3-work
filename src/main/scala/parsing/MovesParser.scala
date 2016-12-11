package parsing

import models.Types.Moves
import models.Move

object MovesParser extends JsonParsingCombinator {
  def jsonToMoves(json: String): Option[Moves] = {
    parseAll(array, json) match {
      case Success(parsedMoves, _) =>
        val validMoves = parsedMoves.filter(m => m.size == 3 && m.contains('x') && m.contains('y') && m.contains('v'))
        if (validMoves.size != parsedMoves.size)
          return None

        Some(validMoves.map(m => Move(m('x').asInstanceOf[Int], m('y').asInstanceOf[Int], m('v').asInstanceOf[Char])))
      case _ => None
    }
  }

  def movesToJson(moves: Moves): String = {
    s"[${moves.map(m => "{\"x\":%s, \"y\":%s, \"v\":\"%s\"}".format(m.x, m.y, m.v)).mkString(",")}]"
  }
}
