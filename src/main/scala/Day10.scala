import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object Day10 extends RegexParsers {

  sealed trait Puttable
  sealed trait Action
  sealed trait Token

  implicit def toPut(puttable: Puttable, low: Puttable, high: Puttable): Put = puttable match {
    case OutputToken(id) => Output(id, low, high)
    case BotToken(id) => Bot(id, low, high)
  }
  case class OutputToken(id: Int) extends Token with Puttable
  case class BotToken(id: Int) extends Token with Puttable
  case class ValueToken(n: Int) extends Token

  case class Pass(id: BotToken, low: Puttable, high: Puttable) extends Token with Action
  case class Goes(value: ValueToken, to: Puttable) extends Token with Action

  def number: Parser[Int] = """0|[1-9]\d*""".r ^^ { _.toInt }

  def basket: Parser[OutputToken] = ("output" ~ number) ^^ { case o ~ n => OutputToken(n) }
  def robot: Parser[BotToken] = ("bot" ~ number) ^^ { case b ~ n => BotToken(n) }
  def value: Parser[ValueToken] = ("value" ~ number) ^^ { case _ ~ n => ValueToken(n) }

  def puttable: Parser[Puttable] = { basket | robot }

  def goes: Parser[Goes] = { value ~ "goes to" ~ puttable } ^^ { case v ~ _ ~ p => Goes(v, p) }
  def pass: Parser[Pass] = { robot ~ "gives low to" ~ puttable ~ "and high to" ~ puttable } ^^ { case r ~ _ ~ p1 ~ _ ~ p2 => Pass(r, p1, p2) }

  def action: Parser[Action] = { goes | pass }

  sealed trait Put extends Puttable {
    def put(v: Int)
  }

  case class Output(id: Int, low: Puttable, high: Puttable) extends Put {
    override def put(v: Int): Unit = {}
  }

  case class Bot(id: Int, low: Puttable, high: Puttable) extends Put {
    override def put(v: Int): Unit = {}
  }

  object ChipManager {
    val puttables = new mutable.HashMap[Puttable, Put]

    def pass(value: Int, to: Puttable): Unit = {

    }

    def add(from: Puttable, low: Puttable, high: Puttable) = {
      puttables getOrElseUpdate(from, toPut(from, low, high))
    }
  }

  def main(args: Array[String]): Unit = {

    io.Source.fromFile("in/10").getLines.foreach(line => {
      parse(action, line).get match {
        case Goes(value, to) => {
          ChipManager pass(value.n, to)
        }
        case Pass(bot, p1, p2) => {
          ChipManager add(bot, p1, p2)
        }
      }
    })
  }
}
