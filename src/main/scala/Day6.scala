
object Day6 {

  def main(args: Array[String]): Unit = {
    val l = io.Source.fromFile("in/6").getLines.toList.transpose(_.toList)
      .map(l => {
        l.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
      })

    val m = io.Source.fromFile("in/6").getLines.toList.transpose(_.toList)
      .map(l => {
        l.groupBy(identity).mapValues(_.size).minBy(_._2)._1
      })


    println(l.mkString)
    println(m.mkString)
  }
}
