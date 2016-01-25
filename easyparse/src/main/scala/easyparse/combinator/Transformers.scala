package easyparse.combinator

/**
  * Created by qso on 15-12-13.
  */
trait Transformers { this: Parsers =>

  case class Mapper[T, R](p: Parser[T], f: T => R) extends Parser[R] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[R] = p(ctx) match {
      case s: MutableResult.Success[T] => success(s, f(s.result), s.next, s.cut)
      case fa: MutableResult.Failure => fa
    }

    override def toString = p.toString
  }

  case class  Filter[T](p: Parser[T], f: T => Boolean) extends Parser[T] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[T] = {
      ctx.logDepth += 1
      val ret = p(ctx) match {
        case fa: MutableResult.Failure => fa
        case su: MutableResult.Success[T] => if (f(su.result)) su else failure(ctx.failure, "The parse result has been filtered out", ctx.input, List(ParseFrame(this, ctx.input)))
      }
      ctx.logDepth -= 1
      ret
    }
  }
}



