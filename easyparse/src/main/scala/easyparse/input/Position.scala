package easyparse.input

trait Position {
  /** The line number refered to by the position, line number start at 1 */
  def line: Int

  def col: Int

  protected def lineContents: String

  override def toString = "" + line + "." + col

  def draw = lineContents + "\n" + lineContents.take(col - 1).map(x => if (x == '\t') x else ' ') + "^"

  def <(that: Position) = line < that.line || line == that.line && col < that.col

  def ==(that: Position) = this.line == that.line && this.col == that.col

  def >(that: Position) = !(this < that || this == that)
}