import scala.util.parsing.combinator.RegexParsers

object Day21 extends RegexParsers {

  trait Action {
    def handle(str: String): String
    def reverseHandle(str: String): String
  }

  trait ActionParser[T] {
    def parser: Parser[T]
  }

  case class SwapPositions(var x: Integer, var y: Integer) extends Action {
    override def handle(str: String): String = {
      val (x1, y1) = if (x < y) (x, y) else (y, x)

      val atX = str.charAt(x1)
      val atY = str.charAt(y1)

      str.substring(0, x1) + atY + str.substring(x1 + 1, y1) + atX + str.substring(y1 + 1)
    }

    override def reverseHandle(str: String): String = handle(str)
  }

  object SwapPositions extends ActionParser[SwapPositions] {
    override def parser: Day21.Parser[SwapPositions] = {
      "swap position" ~ int ~ "with position" ~ int
    } ^^ { case _ ~ x ~ _ ~ y => SwapPositions(x, y) }
  }

  case class SwapOccurence(x: Char, y: Char) extends Action {
    override def handle(str: String): String = {
      SwapPositions(str.indexOf(x), str.indexOf(y)).handle(str)
    }

    override def reverseHandle(str: String): String = handle(str)
  }

  object SwapOccurence extends ActionParser[SwapOccurence] {
    override def parser: Day21.Parser[SwapOccurence] = {
      "swap letter" ~ char ~ "with letter" ~ char
    } ^^ { case _ ~ x ~ _ ~ y => SwapOccurence(x, y) }
  }

  case class RotateLeft(x: Integer) extends Action {
    override def handle(str: String): String = {
      val y = x % str.length
      str.substring(y) + str.substring(0, y)
    }

    override def reverseHandle(str: String): String = RotateRight(x).handle(str)
  }

  object RotateLeft extends ActionParser[RotateLeft] {
    override def parser: Day21.Parser[RotateLeft] = {
      "rotate left" ~ int ~ "step[s]*".r
    } ^^ { case _ ~ x ~ _ => RotateLeft(x) }
  }

  case class RotateRight(x: Integer) extends Action {
    override def handle(str: String): String = {
      val y = str.length - (x % str.length)
      RotateLeft(y).handle(str)
    }

    override def reverseHandle(str: String): String = RotateLeft(x).handle(str)
  }

  object RotateRight extends ActionParser[RotateRight] {
    override def parser: Day21.Parser[RotateRight] = {
      "rotate right" ~ int ~ "step[s]*".r
    } ^^ { case _ ~ x ~ _ => RotateRight(x) }
  }

  case class RotateBasedOnX(x: Char) extends Action {
    override def handle(str: String): String = {
      val idx = str.indexOf(x)
      val y = idx + 1 + (if (idx >= 4) 1 else 0)
      RotateRight(y).handle(str)
    }

    override def reverseHandle(str: String): String = {
      str.toCharArray.permutations.map(_.mkString)
          .toSeq.find(s => handle(s) == str).get
    }
  }

  object RotateBasedOnX extends ActionParser[RotateBasedOnX] {
    override def parser: Day21.Parser[RotateBasedOnX] = {
      "rotate based on position of letter" ~ char
    } ^^ { case _ ~ x => RotateBasedOnX(x) }
  }

  case class ReverseInterval(x: Integer, y: Integer) extends Action {
    override def handle(str: String): String = {
      val (x1, y1) = (Math.min(x, y), Math.max(x, y))

      str.substring(0, x1) + str.substring(x1, y1 + 1).reverse + str.substring(y1 + 1)
    }

    override def reverseHandle(str: String): String = handle(str)
  }

  object ReverseInterval extends ActionParser[ReverseInterval] {
    override def parser: Day21.Parser[ReverseInterval] = {
      "reverse positions" ~ int ~ "through" ~ int
    } ^^ { case _ ~ x ~ _ ~ y => ReverseInterval(x, y) }
  }

  case class MovePosition(x: Integer, y: Integer) extends Action {
    override def handle(str: String): String = {
      val ch = str.charAt(x)
      val str1 = str.substring(0, x) + str.substring(x + 1)
      str1.substring(0, y) + ch + str1.substring(y)
    }

    override def reverseHandle(str: String): String = MovePosition(y, x).handle(str)
  }

  object MovePosition extends ActionParser[MovePosition] {
    override def parser: Day21.Parser[MovePosition] = {
      "move position" ~ int ~ "to position" ~ int
    } ^^ { case _ ~ x ~ _ ~ y => MovePosition(x, y) }
  }

  def char: Parser[Char] = """\w""".r ^^ (c => c.charAt(0))

  def int: Parser[Integer] = """\d+""".r ^^ (i => i.toInt)

  def Parse: Parser[Action] = {
    SwapPositions.parser | SwapOccurence.parser | RotateLeft.parser | RotateRight.parser | RotateBasedOnX.parser | ReverseInterval.parser | MovePosition.parser
  }

  val Input = "abcdefgh"
  val Unscramble = "fbgdceah"

  def main(args: Array[String]): Unit = {

    var x = io.Source.fromFile("in/21").getLines.map(
      line => {
        val p = parse(Parse, line)
        p.get
      }
    ).foldLeft(Input) {
      (pr, f) => {
        f.handle(pr)
      }
    }
    println(x)

    //TODO: reverse
    var y = io.Source.fromFile("in/21").getLines.toSeq.reverse.map(
      line => {
        val p = parse(Parse, line)
        p.get
      }
    ).foldLeft(Unscramble) {
      (pr, f) => {
        f.reverseHandle(pr)
      }
    }
    println(y)
  }
}
