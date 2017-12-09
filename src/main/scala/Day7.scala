
object Day7 {

  def isEven(number: Int) : Boolean = number % 2 == 0
  def isOdd(number: Int) = !isEven(number)

  def hasAbba(s: String) = {
    (0 to s.length-4).map(idx => s.substring(idx, idx + 4)).exists(isAbba)
  }

  def isAbba(s: String): Boolean = {
    s.length == 4 && s{0} == s{3} && s{1} == s{2} && s{0} != s{1}
  }

  def reverseAba(s: String): String = {
    new StringBuilder().append(s{1}).append(s{0}).append(s{1}).mkString
  }

  def getAba(s: String): List[String] = {
    (0 to s.length-3).map(idx => s.substring(idx, idx + 3)).filter(isAba).toList
  }

  def isAba(s: String): Boolean = {
    s.length == 3 && s{0} == s{2} && s{0} != s{1}
  }

  def main(args: Array[String]): Unit = {

    val x = io.Source.fromFile("in/7").getLines.toList
      .filter(str => {
        val groups = str.split("\\[|\\]").zipWithIndex.groupBy(_._2 % 2).mapValues(_.map(_._1).toList)
        groups.get(0).get.exists(hasAbba) && groups.get(1).get.forall(!hasAbba(_))
      })

    val y = io.Source.fromFile("in/7").getLines.toList
      .filter(str => {
        val groups = str.split("\\[|\\]").zipWithIndex.groupBy(_._2 % 2).mapValues(_.map(_._1).toList)
        val allAba = groups.get(0).get.flatMap(getAba)
        !allAba.isEmpty && groups.get(1).get.exists(s => { allAba.exists(aba => s.contains(reverseAba(aba))) })
      })

    println(x.length)
    println(y.length)
  }
}
