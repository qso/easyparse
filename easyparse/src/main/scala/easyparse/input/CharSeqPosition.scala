package easyparse.input

import scala.collection.mutable.ArrayBuffer

case class CharSeqPosition(source: java.lang.CharSequence, offset: Int) extends Position {

  private lazy val lineIndex: Array[Int] = {
    val lineStarts = new ArrayBuffer[Int]
    lineStarts += 0
    for (i <- 0 until source.length)
      if (source.charAt(i) == '\n') lineStarts += i + 1
    lineStarts += source.length
    lineStarts.toArray
  }

  def line: Int =  {
    var lo = 0
    var hi = lineIndex.length - 1
    while (lo + 1 < hi) {
      val mid = (lo + hi) / 2
      if (offset < lineIndex(mid)) hi = mid
      else lo = mid
    }
    lo + 1
  }

  def col: Int = offset - lineIndex(line - 1) + 1

  def lineContents: String = source.subSequence(lineIndex(line - 1), lineIndex(line)).toString

  override def <(that: Position) = that match {
    case CharSeqPosition(_, thatOffset) => this.offset < thatOffset
    case _ => super.<(that)
  }
}