import scala.collection.mutable

object Day13 {

  val Input = 1362
  val Goal = (31, 39)

  val ToCheck = mutable.Queue.empty[(Int, Int, Int)]
  val Visited = mutable.HashSet.empty[(Int, Int)]

  def countOnes(n: Int): Int = {
    if (n != 0) (n & 0x1) + countOnes(n >> 1) else 0
  }

  def calcStepValue(x: Int, y: Int): Int = {
    x*x + 3*x + 2*x*y + y + y*y + Input
  }

  def steppable(x: Int, y: Int): Boolean = {
    (x >= 0) && (y >= 0) && (countOnes(calcStepValue(x, y)) % 2 == 0)
  }

  def generateVisitable(x: Int, y: Int): List[(Int, Int)] = {
    val PossibleSteps = List((1,0), (0,1), (-1,0), (0,-1))
    PossibleSteps.map(s => (x + s._1, y + s._2)).filter(s => steppable(s._1, s._2)).filter(!Visited.contains(_))
  }

  def main(args: Array[String]): Unit = {

    //#1
    ToCheck += ((1,1,0))

    while (ToCheck.nonEmpty) {
      val (x, y, steps) = ToCheck.dequeue

      if ((x, y) == Goal) {
        println(steps)
        ToCheck.clear()
      } else {
        ToCheck ++= generateVisitable(x, y).map(s => (s._1, s._2, steps + 1))
        Visited += ((x, y))
      }
    }

    Visited.clear()

    //#2
    ToCheck += ((1,1,0))

    while(ToCheck.nonEmpty) {
      val (x, y, steps) = ToCheck.dequeue

      if (steps <= 50) {
        Visited += ((x, y))
        ToCheck ++= generateVisitable(x, y).map(s => (s._1, s._2, steps + 1))
      }
    }

    println(Visited.size)
  }
}
