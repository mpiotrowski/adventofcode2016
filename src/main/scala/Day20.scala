

object Day20 {

  class Interval(val low: Long, val high: Long) {
    override def toString: String = this.low + " " + this.high
  }

  def testBlocked(ip: Long, interval: Interval): Boolean = {
    interval.low <= ip && ip <= interval.high
  }

  def main(args: Array[String]): Unit = {

    val intervals = io.Source.fromFile("in/20").getLines.map(line => {
      val l = line.split("-")
      new Interval(l{0}.toLong, l{1}.toLong)
    }).toList.sortWith((i1: Interval, i2: Interval) => {
      i1.low <= i2.low && i1.high <= i2.high
    })

    val candidates = intervals.map(_.high + 1).filter(_ <= 4294967295l)

    val found = candidates
      .filter(candidate => {
        !intervals.exists(testBlocked(candidate, _))
      })

    println(found.min)

    println(found.size)
  }
}
