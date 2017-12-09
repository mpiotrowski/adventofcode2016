import scala.util.matching.Regex

object Day22 {

  class Node(val x: Int, val y: Int, val size: Int, val used: Int)

  val Line = new Regex("x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T.*", "x", "y", "all", "used")

  def main(args: Array[String]): Unit = {

    val nodes = io.Source.fromFile("in/22").getLines
      .map(line => {
        val m = Line.findFirstMatchIn(line)
        val x = m.get.group("x").toInt
        val y = m.get.group("y").toInt
        val all = m.get.group("all").toInt
        val used = m.get.group("used").toInt

        new Node(x, y, all, used)
      })

    val pairs = for(x <- nodes; y <- nodes) yield (x, y)
    val x = pairs
      .map(pair => {
        val (n1, n2) = pair
        n1.used != 0 && (n1.x != n2.x && n1.y != n2.y) && (n1.size - n1.used <= n2.used)
      })
      .map(b => if(b) 1 else 0).sum

    println(x)
  }
}
