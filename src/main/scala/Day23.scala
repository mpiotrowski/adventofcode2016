import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object Day23 extends RegexParsers {


  object State {
    val registers = mutable.HashMap[Char, Int]('a' -> 12)
    var position = 0
    val instructions = mutable.MutableList[Action]()
  }

  sealed trait Action {
    def action()
    def toggle(): Action
  }
  sealed trait Evaluable {
    def getVal(): Int
    def setVal(v: Int): Unit
  }
  case class Value(i: Int) extends Evaluable {
    override def getVal() = { i }
    override def setVal(i: Int) = {}
  }
  case class Register(ch: Char) extends Evaluable {
    override def getVal(): Int = State.registers getOrElseUpdate(ch, 0)
    override def setVal(i: Int): Unit = State.registers update(ch, i)
  }
  case class Cpy(from: Evaluable, to: Evaluable) extends Action {
    override def action(): Unit = {
      to.setVal(from.getVal)
      State.position += 1
    }

    override def toggle(): Action = Jnz(from, to)
  }
  case class Inc(of: Evaluable) extends Action {
    override def action(): Unit = {
      of.setVal(of.getVal + 1)
      State.position += 1
    }

    override def toggle(): Action = Dec(of)
  }
  case class Dec(of: Evaluable) extends Action {
    override def action(): Unit = {
      of.setVal(of.getVal - 1)
      State.position += 1
    }

    override def toggle(): Action = Inc(of)
  }
  case class Jnz(of: Evaluable, to: Evaluable) extends Action {
    override def action(): Unit = {
      if (of.getVal != 0) {
        State.position += to.getVal
      } else {
        State.position += 1
      }
    }

    override def toggle(): Action = Cpy(of, to)
  }
  case class Tgl(of: Evaluable) extends Action {
    override def action(): Unit = {
      val placed = State.position + of.getVal
      if(placed >= 0 && placed < State.instructions.length) {
        State.instructions{placed} = State.instructions{placed}.toggle
      }
      State.position += 1
    }

    override def toggle(): Action = Inc(of)
  }

  def number: Parser[Value] = """(-)?\d+""".r ^^ (i => Value(i.toInt))
  def register: Parser[Register] = """[a-d]""".r ^^ (c => Register(c.charAt(0)))

  def evaluable: Parser[Evaluable] = { number | register }

  def cpy: Parser[Cpy] = { "cpy" ~ evaluable ~ evaluable } ^^ { case _ ~ v ~ r => Cpy(v, r) }
  def inc: Parser[Inc] = { "inc" ~ evaluable } ^^ { case _ ~ r => Inc(r) }
  def dec: Parser[Dec] = { "dec" ~ evaluable } ^^ { case _ ~ r => Dec(r) }
  def jnz: Parser[Jnz] = { "jnz" ~ evaluable ~ evaluable } ^^ { case _ ~ r ~ v => Jnz(r, v) }
  def tgl: Parser[Tgl] = { "tgl" ~ evaluable } ^^ { case _ ~ e => Tgl(e) }

  def action: Parser[Action] = { cpy | inc | dec | jnz | tgl }

  def main(args: Array[String]): Unit = {

    val ins = io.Source.fromFile("in/23").getLines.map(line => parse(action, line).get).toArray
    State.instructions ++= ins
    while(State.position < State.instructions.length) {
      State.instructions{State.position}.action()
    }
    println(State.registers)
  }
}
