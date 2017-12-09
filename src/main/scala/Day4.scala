import scala.collection.mutable
import scala.util.matching.Regex

object Day4 {

  val RoomCheck = new Regex("""((?:[a-z]+-)+)(\d+)\[([a-z]+)\]""", "roomLetters", "roomNumber", "checksum")

  def check(line: String): Boolean = {
    val check = RoomCheck findFirstMatchIn line
    val roomNumber = check.get.group("roomNumber").toInt

    val letters = check.get.group("roomLetters").replace("-", "")
      .foldLeft(mutable.Map[Char,Int]())((map, letter) => {
        map.updated(letter, map.getOrElse(letter, 0) + 1)
      })
      .toList
      .sortWith((a,b) => (a._2 > b._2) || (a._1 < b._1 && a._2 == b._2))
      .map(_._1)

    check.get.group("checksum").zip(letters).forall(pair => pair._1 == pair._2)
  }

  def decrypt(line: String): (String, Int) = {
    val check = RoomCheck findFirstMatchIn line
    val roomNumber = check.get.group("roomNumber").toInt

    (check.get.group("roomLetters").replace("-", "")
      .map((letter) => (letter - 'a' + roomNumber) % 26 + 'a')
      .map(_.toChar)
      .toString(), roomNumber)
  }

  def main(args: Array[String]): Unit = {

    println(io.Source.fromFile("in/4").getLines.filter(check).map(decrypt).toVector)
  }
}
