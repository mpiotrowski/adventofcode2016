
object Day15 {

  // find startTime: startTime + ordinal + startPos = 0 mod length

  // (length, startPos)
  val Disks1 = (17, 15) :: (3, 2) :: (19, 4) :: (13, 2) :: (7, 2) :: (5, 0) :: Nil
  val Disks2 = (17, 15) :: (3, 2) :: (19, 4) :: (13, 2) :: (7, 2) :: (5, 0) :: (11, 0) :: Nil

  def main(args: Array[String]): Unit = {

    // #1
    val startTime1 = Stream.from(0)
      .find(st => {
        Disks1.zipWithIndex.forall {
          case ((l, sp), i) => {
            (st + i + 1 + sp) % l == 0
          }
        }
      })

    println(startTime1)

    // #1
    val startTime2 = Stream.from(0)
      .find(st => {
        Disks2.zipWithIndex.forall {
          case ((l, sp), i) => {
            (st + i + 1 + sp) % l == 0
          }
        }
      })

    println(startTime2)
  }
}