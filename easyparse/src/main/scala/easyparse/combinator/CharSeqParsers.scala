package easyparse.combinator

import easyparse.input.{PagedSeqReader, CharSeqReader}

import scala.collection.immutable.PagedSeq
import scala.util.matching.Regex

/**
  * Created by qso on 16-1-12.
  */
class CharSeqParsers extends Parsers {
  type Elem = Char

  protected val whiteSpace = """\s+""".r

  def skipWhiteSpace: Boolean = true

  /** Method called to handle whitespace before parsers.
    *
    *  It checks `skipWhitespace` and, if true, skips anything
    *  matching `whiteSpace` starting from the current offset.
    *
    *  @param source  The input being parsed.
    *  @param offset  The offset into `source` from which to match.
    *  @return        The offset to be used for the next parser.
    */
  protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
    if (skipWhiteSpace)
      whiteSpace findPrefixMatchOf source.subSequence(offset, source.length) match {
        case Some(matched) => offset + matched.end
        case None => offset
      }
    else
      offset


  implicit def literal(s: String): Parser[String] = Literal(s)

  case class Literal(s: String) extends Parser[String] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[String] = {
      val source = ctx.input.source
      val offset = ctx.input.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start
      while (i < s.length && j < source.length &&  s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length) success(ctx.success, source.subSequence(start, j).toString, ctx.input.drop(j - offset))
      else {
        val found = if (j == source.length) "end of source" else "\'"+source.charAt(j)+"\'"
        failure(ctx.failure, s"\'${s.charAt(i)}\' expected but $found found",ctx.input, List(ParseFrame(this, ctx.input)))
      }
    }

    override  def toString = "\"" + s +"\""
  }

def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] = parse(p, new CharSeqReader(in))

def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = parse(p, new PagedSeqReader(PagedSeq.fromReader(in)))

def parseAll[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] = parseAll(p, new CharSeqReader(in))

def parseAll[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = parseAll(p, new PagedSeqReader(PagedSeq.fromReader(in)))
}
