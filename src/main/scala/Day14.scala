import java.security.MessageDigest

import scala.collection.mutable

object Day14 {

  val Input = "yjdafjpo"

  type Hash = String

  val hashes = mutable.HashMap.empty[Int, Hash]

  def calcHash(hash: Hash): Hash = {
    MessageDigest.getInstance("MD5").digest(hash.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }

  def hash(idx: Int): Hash = {
      hashes getOrElseUpdate(idx, (1 to 2017).foldLeft(Input + idx)((h, _) => calcHash(h)))
  }

  def allSame(group: String): Boolean = {
    group.toSet.size == 1
  }

  def hasSame(hash: Hash, size: Int): Boolean = {
    (0 until size).flatMap(s => hash.drop(s).grouped(size)).toStream.filter(_.length == size).exists(allSame)
  }

  def getSame(hash: Hash, size: Int): Seq[String] = {
    (0 until size).flatMap(s => hash.drop(s).grouped(size)).toStream.filter(_.length == size).filter(allSame)
  }

  def sameContains(same: Seq[String], ch: Char): Boolean = {
    same.exists(s => s.contains(ch))
  }

  def main(args: Array[String]): Unit = {

    (0 until Int.MaxValue)
      .toStream
      .map(i => (hash(i), i))
      .filter(x => {
        hasSame(x._1, 3)
      })
      .filter(x => {
        val ch = getSame(x._1, 3).head.charAt(0)
        (x._2 + 1 to x._2 + 1000).map(hash).map(getSame(_, 5)).exists(sameContains(_, ch))
      })
      .take(64)
      .map(_._2)
      .foreach(i => println(i))
  }
}
