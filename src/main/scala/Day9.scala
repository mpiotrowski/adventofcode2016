import scala.util.parsing.combinator.RegexParsers

object Day9 extends RegexParsers {

  case class Marker(length: Int, repeat: Int)

  sealed trait Evaluable {
    def evaluate(): String
    def totalLength(): Long
  }
  case class Letter(c: Char) extends Evaluable {
    override def evaluate(): String = c.toString

    override def totalLength(): Long = 1l
  }
  case class Sequence(repeat: Int, seq: String) extends Evaluable {
    override def evaluate(): String = (0 until repeat).map(_ => { seq }).mkString

    override def totalLength(): Long = {
      repeat.toLong * parse(decode, seq).get.view.map(_.totalLength).sum
    }
  }

  def number: Parser[Int] = "\\d+".r ^^ { _.toInt }
  def char: Parser[Letter] = "[A-Z]".r ^^ { c => Letter(c.charAt(0)) }
  def seq(length: Int, repeat: Int): Parser[Sequence] = (".{" + length + "}").r ^^ { Sequence(repeat, _) }
  def marker: Parser[Marker] = { "(" ~ number ~ "x" ~ number ~ ")" } ^^ { case _ ~ l ~ _ ~ r ~ _ => Marker(l, r) }

  def seqWithMarker: Parser[Sequence] = marker into (marker => seq(marker.length, marker.repeat))

  def decode: Parser[List[Evaluable]] = { (seqWithMarker | char) * } ^^ (l => l)

  def tryDecode(input: String): String = {
    parse(decode, input)
        .get
        .map(_.evaluate())
        .mkString
  }



  def main(args: Array[String]): Unit = {

    val in = io.Source.fromFile("in/9").getLines.toList.head

//    println(tryDecode(in).length)

    val totalLen = parse(decode, in).get.view.map(_.totalLength).sum
    println(totalLen)
  }
}
