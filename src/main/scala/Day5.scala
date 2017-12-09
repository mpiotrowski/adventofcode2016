import java.security.MessageDigest

object Day5 {

  val Input = "wtnhxymk";

  def calcHash(idx: Int) = {
    MessageDigest.getInstance("MD5").digest((Input + idx).getBytes)
  }

  def main(args: Array[String]) = {
    val x = (0 to Int.MaxValue - 1).iterator
      .map(n => calcHash(n).toList)
      .filter(hash => {
        hash{0} == 0 && hash{1} == 0 && (hash{2} & 0xF0) == 0
      })
      .map(_{2} & 0x0F)
      .take(8)
      .toList

    x.foreach(println)

    var found = Array.fill[Option[Int]](8){Option.empty}

    (0 to Int.MaxValue - 1).iterator
      .map(n => calcHash(n).toList)
      .filter(hash => {
        hash{0} == 0 && hash{1} == 0 && (hash{2} & 0xF0) == 0
      })
      .takeWhile(hash => {
        val position = hash{2} & 0x0F
        val value = (hash{3} & 0xF0) >> 4

        if (position < 8 && found{position}.isEmpty) found{position} = Some(value)

        found.contains(Option.empty)
      })

    found.map(_.get).foreach(println)
  }
}
