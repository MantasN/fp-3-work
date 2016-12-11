package parsing

import scala.util.parsing.combinator._

class JsonParsingCombinator extends JavaTokenParsers {
  def array: Parser[List[Map[Char, Any]]] =
    "["~> repsep(obj, ",") <~"]"

  def obj: Parser[Map[Char, Any]] =
    "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)

  def member: Parser[(Char, Any)] =
    coord | player

  def coord: Parser[(Char, Int)] =
    ("\""+"""(x|X|y|Y)"""+"\"").r~":"~"""(0|1|2)""".r ^^ {
      case coord~":"~pos => (coord.charAt(1).toLower, pos.toInt)
    }

  def player: Parser[(Char, Char)] =
    ("\""+"""(v|V)"""+"\"").r~":"~("\""+"""(x|X|o|O)"""+"\"").r ^^ {
      case v~":"~player => (v.charAt(1).toLower, player.charAt(1).toLower)
    }
}
