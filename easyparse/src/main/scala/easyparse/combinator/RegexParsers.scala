package easyparse.combinator


import scala.util.matching.Regex

/**
  * Created by qso on 16-1-20.
  */
class RegexParsers extends CharSeqParsers {
  implicit def regexer(r: Regex): Parser[String] = Regexer(r)

  case class Regexer(r: Regex) extends Parser[String] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[String] = {
      val source = ctx.input.source
      val offset = ctx.input.offset
      val start = handleWhiteSpace(source, offset)
      r findPrefixMatchOf source.subSequence(start, source.length) match {
        case Some(matched) =>
          success(ctx.success, source.subSequence(start, start + matched.end).toString, ctx.input.drop(start + matched.end - offset))
        case None =>
          val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
          failure(ctx.failure, s"string matching regex `${r} expected but `${found} found", ctx.input.drop(start - offset), List(ParseFrame(this, ctx.input)))
      }
    }
  }

}
