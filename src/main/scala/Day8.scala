import scala.util.matching.Regex

object Day8 {

  val EmptyBoard = Array.ofDim[Boolean](6, 50)

  val Rect = """rect (\d+)x(\d+)"""
  val RectR = new Regex(Rect)
  val RotRow = """rotate row y=(\d+) by (\d+)"""
  val RotRowR = new Regex(RotRow)
  val RotCol = """rotate column x=(\d+) by (\d+)"""
  val RotColR = new Regex(RotCol)

  def rotate(list: Array[Boolean], n: Int) = list.drop(list.length - n) ++ list.take(list.length - n)

  def main(args: Array[String]): Unit = {

    println(io.Source.fromFile("in/8").getLines.toList.foldLeft(EmptyBoard)((B: Array[Array[Boolean]], line: String) => {
      if (line.matches(Rect)) {
        val rect = RectR findFirstMatchIn line
        (0 until rect.get.group(1).toInt).foreach(x => {
          (0 until rect.get.group(2).toInt).foreach(y => { B{y}{x} = true })
        })
      } else if(line.matches(RotRow)) {
        val rot = RotRowR findFirstMatchIn line

        B{rot.get.group(1).toInt} = rotate(B{rot.get.group(1).toInt}, rot.get.group(2).toInt)
      } else {
        val rot = RotColR findFirstMatchIn line
        val n = rot.get.group(1).toInt

        val col = rotate(B.map(_{n}), rot.get.group(2).toInt)
        B.zipWithIndex.foreach{ case(ar: Array[Boolean], i: Int) => ar.update(n, col{i}) }
      }
      B
      //Replace with sums for #1
    }).map(_.map(if (_) "X" else "0").mkString.concat("\n")).mkString)
  }

}
