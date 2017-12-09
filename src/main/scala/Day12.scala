import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object Day12 extends RegexParsers {

  object State {
    val registers = mutable.HashMap[Char, Int]('c' -> 1)
    var position = 0
  }

  sealed trait Action {
    def action()
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
  }
  case class Inc(of: Evaluable) extends Action {
    override def action(): Unit = {
      of.setVal(of.getVal + 1)
      State.position += 1
    }
  }
  case class Dec(of: Evaluable) extends Action {
    override def action(): Unit = {
      of.setVal(of.getVal - 1)
      State.position += 1
    }
  }
  case class Jmp(of: Evaluable, to: Evaluable) extends Action {
    override def action(): Unit = {
      if (of.getVal != 0) {
        State.position += to.getVal
      } else {
        State.position += 1
      }
    }
  }

  def number: Parser[Value] = """(-)?\d+""".r ^^ (i => Value(i.toInt))
  def register: Parser[Register] = """[a-d]""".r ^^ (c => Register(c.charAt(0)))

  def evaluable: Parser[Evaluable] = { number | register }

  def cpy: Parser[Cpy] = { "cpy" ~ evaluable ~ evaluable } ^^ { case _ ~ v ~ r => Cpy(v, r) }
  def inc: Parser[Inc] = { "inc" ~ evaluable } ^^ { case _ ~ r => Inc(r) }
  def dec: Parser[Dec] = { "dec" ~ evaluable } ^^ { case _ ~ r => Dec(r) }
  def jmp: Parser[Jmp] = { "jnz" ~ evaluable ~ evaluable } ^^ { case _ ~ r ~ v => Jmp(r, v) }

  def action: Parser[Action] = { cpy | inc | dec | jmp }

  def main(args: Array[String]): Unit = {

    val ins = io.Source.fromFile("in/12").getLines.map(line => parse(action, line).get).toArray
    while(State.position < ins.length) {
      ins{State.position}.action()
    }
    println(State.registers)
  }
}
