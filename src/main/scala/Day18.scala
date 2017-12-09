

object Day18 {

  val Input = "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^."

  abstract class Tile(val v: Int)

  case class Safe() extends Tile(1) {
    override def toString: String = "."
  }
  case class Trap() extends Tile(0) {
    override def toString: String = "^"
  }

  class Row(l : List[Tile]){
    def nextRow(): Row = {
      val fullRow = Safe() +: l :+ Safe()
      val toTake = fullRow.length - 2
      (fullRow.take(toTake), fullRow.slice(1, toTake + 1), fullRow.takeRight(toTake)).zipped.toList.map {
        case (Trap(), _, Trap()) => Safe()
        case (Safe(), _, Safe()) => Safe()
        case _ => Trap()
      }
    }

    override def toString: String = l.map(_.toString).mkString

    def calcSafe = l.map(_.v).sum
  }
  implicit def row(l: List[Tile]): Row = new Row(l)

  def translate(input: String): Row = input.map { case '.' => Safe() case '^' => Trap() } toList

  def howMany(first: Row)(til: Int): Int = {
    (1 until til).foldLeft((first, first.calcSafe))((r, _) => {
      val (row, sum) = r
      val newRow = row nextRow()
      (newRow, sum + newRow.calcSafe)
    })._2
  }

  def main(args: Array[String]): Unit = {

    val first = translate(Input)
    val many = howMany(first)(_)

    //#1
    println(many(40))
    //#2
    println(many(400000))
  }
}
