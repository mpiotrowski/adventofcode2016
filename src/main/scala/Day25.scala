import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object Day25 extends RegexParsers {
  object State {
    val registers = mutable.HashMap[Char, Int]()
    var position = 0
    val instructions = mutable.MutableList[Action]()

    var lastOut: Int = -1
    var aToCheck = -1

    def out(v: Int): Unit = {
      if(v == lastOut) {
        reset
      } else {
        lastOut = v
      }
    }

    def reset(): Unit = {
      aToCheck += 1
      println("Checking", aToCheck)
      registers.clear()
      registers update ('a', aToCheck)
      position = -1
      lastOut = -1
    }
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
  case class Jnz(of: Evaluable, to: Evaluable) extends Action {
    override def action(): Unit = {
      if (of.getVal != 0) {
        State.position += to.getVal
      } else {
        State.position += 1
      }
    }
  }
  case class Out(of: Evaluable) extends Action {
    override def action(): Unit = {
      State.out(of.getVal)
      State.position += 1
    }
  }

  def number: Parser[Value] = """(-)?\d+""".r ^^ (i => Value(i.toInt))
  def register: Parser[Register] = """[a-d]""".r ^^ (c => Register(c.charAt(0)))

  def evaluable: Parser[Evaluable] = { number | register }

  def cpy: Parser[Cpy] = { "cpy" ~ evaluable ~ evaluable } ^^ { case _ ~ v ~ r => Cpy(v, r) }
  def inc: Parser[Inc] = { "inc" ~ evaluable } ^^ { case _ ~ r => Inc(r) }
  def dec: Parser[Dec] = { "dec" ~ evaluable } ^^ { case _ ~ r => Dec(r) }
  def jnz: Parser[Jnz] = { "jnz" ~ evaluable ~ evaluable } ^^ { case _ ~ r ~ v => Jnz(r, v) }
  def out: Parser[Out] = { "out" ~ evaluable } ^^ { case _ ~ e => Out(e) }

  def action: Parser[Action] = { cpy | inc | dec | jnz | out }

  def main(args: Array[String]): Unit = {

    val ins = io.Source.fromFile("in/25").getLines.map(line => parse(action, line).get).toArray
    State.instructions ++= ins
    State reset()
    while(State.position < State.instructions.length) {
      State.instructions{State.position}.action()
    }
    println(State.registers)
  }

}
