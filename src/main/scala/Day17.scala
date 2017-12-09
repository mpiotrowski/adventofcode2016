import java.security.MessageDigest

object Day17 {

  val Input = "qljzarfv"

  type Hash = Array[Byte]
  type Position = (Int, Int)
  type Moves = Array[Char]

  def sanitize(i: Int) = if (i > 0xF) (i & 0xF0) >> 4 else i & 0xF

  def calcHash(str: String) = {
    MessageDigest.getInstance("MD5").digest(str.getBytes)
  }

  def canGoTop(hash: Hash, pos: Position): Boolean = ((hash{0} & 0xF0) >> 4) > 10 && pos._2 > 0
  def canGoDown(hash: Hash, pos: Position): Boolean = (hash{0} & 0xF) > 10 && pos._2 < 3
  def canGoLeft(hash: Hash, pos: Position): Boolean = ((hash{1} & 0xF0) >> 4) > 10 && pos._1 > 0
  def canGoRight(hash: Hash, pos: Position): Boolean = (hash{1} & 0xF) > 10 && pos._1 < 3

  def tryGoTop(hash: Hash, pos: Position): Moves = if (canGoTop(hash, pos)) Array('U') else Array()
  def tryGoDown(hash: Hash, pos: Position): Moves = if (canGoDown(hash, pos)) Array('D') else Array()
  def tryGoLeft(hash: Hash, pos: Position): Moves = if (canGoLeft(hash, pos)) Array('L') else Array()
  def tryGoRight(hash: Hash, pos: Position): Moves = if (canGoRight(hash, pos)) Array('R') else Array()

  def isRightPosition(pos: Position) = pos._1 == 3 && pos._2 == 3

  def possibleMoves(prevMoves: Moves, position: Position): Moves = {
    val hash = calcHash(Input + prevMoves.mkString)
    tryGoDown(hash, position) ++ tryGoRight(hash, position) ++ tryGoTop(hash, position) ++ tryGoLeft(hash, position)
  }

  def move(move: Char, position: Position): Position = move match {
    case 'U' => (position._1, position._2 - 1)
    case 'D' => (position._1, position._2 + 1)
    case 'L' => (position._1 - 1, position._2)
    case 'R' => (position._1 + 1, position._2)
  }

  def oneStep(prevMoves: Moves, position: Position): Array[Moves] = {
    if (isRightPosition(position))
      Array(prevMoves)
    else
      possibleMoves(prevMoves, position).flatMap(map(prevMoves, position))
  }

  def map(prevMoves: Moves, position: Position)(m: Char): Array[Moves] = {
    oneStep(prevMoves :+ m, move(m, position))
  }

  def main(args: Array[String]): Unit = {

    //#1
    println(oneStep(Array(), (0,0)).minBy(_.length).mkString)
    //#2
    println(oneStep(Array(), (0,0)).maxBy(_.length).length)
  }
}
