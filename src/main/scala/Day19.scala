

object Day19 {

  val Num = 3018458

  //#1
  def skip(x: List[Int]):Option[Int] = skip1(List(), x)

  def skip1(l1: List[Int], l2: List[Int]): Option[Int] = {
    (l1, l2) match {
      case (Nil, Nil) => None
      case (Nil, x :: Nil) => Some(x)
      case (l, x::_::xs) => skip1(x::l, xs)
      case (l, xs) => skip1(Nil, xs ++ l.reverse)
    }
  }

  //#2 - run only if you want to spend forever on finding the answer
  //I wasn't able to find appropriate Scala data structure that would provide fast split and merge actions
  def sskip(x: List[Int]): Int = {
    if(x.length % 100 == 0) println(x.length)
    x match {
      case h::Nil => h
      case h::r => {
        val toSkip = (r.length / 2) + (r.length % 2)
        sskip((r.take(toSkip - 1) ++ r.drop(toSkip)) :+ h)
      }
    }
  }

  def main(args: Array[String]) {

    println(skip((1 to Num).toList))

    println(sskip((1 to Num).toList))
  }
}
