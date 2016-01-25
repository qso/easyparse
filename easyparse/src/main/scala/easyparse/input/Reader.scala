package easyparse.input

abstract class Reader[+T] {

  def source: java.lang.CharSequence = throw new NoSuchMethodError("not a char sequence reader")

  def offset: Int = throw new NoSuchMethodError("not a char sequence reader")

  def first: T

  def rest: Reader[T]

  def drop(n: Int) = {
    var r: Reader[T] = this
    var cnt = n
    while (cnt > 0) {
      r = r.rest
      cnt -= 1
    }
    r
  }

  def pos: Position

  def atEnd: Boolean

  def atStart: Boolean
}