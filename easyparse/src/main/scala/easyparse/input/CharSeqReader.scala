package easyparse.input

object CharSeqReader {
  final val EofCh = '\u001a'
}

class CharSeqReader(override val source: java.lang.CharSequence, override val offset: Int) extends Reader[Char] {

  def this(source: java.lang.CharSequence) = this(source, 0)

  def first = if (offset < source.length) source.charAt(offset) else CharSeqReader.EofCh

  def rest = if (offset < source.length) new CharSeqReader(source, offset + 1) else this

  def pos: Position = CharSeqPosition(source, offset)

  def atEnd: Boolean = offset >= source.length

  def atStart: Boolean = offset == 0

  override def drop(n: Int) = new CharSeqReader(source, offset + n)
}