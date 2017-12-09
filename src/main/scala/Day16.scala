
object Day16 {

  val Input = "10011111011011001"
  val RequiredLength = 35651584

  def mirror(s: String): String = s.reverse.map { case '0' => '1' case '1' => '0' }

  def buildUp(s: String): String = s + '0' + mirror(s)

  def buildUntil(s: String): String = {
    if (s.length < RequiredLength) buildUntil(buildUp(s)) else s
  }

  def hash(s: String): String = {
    if (s.length % 2 == 0) {
      hash(s.grouped(2).map {
        case "00" | "11" => "1"
        case _ => "0"
      } mkString)
    } else {
      s
    }
  }

  def main(args: Array[String]): Unit = {

    val x = List(Input).map(buildUntil).head.take(RequiredLength)
//    println(x)
    println(hash(x))
  }
}
