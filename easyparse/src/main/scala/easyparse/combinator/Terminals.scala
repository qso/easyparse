package easyparse
package combinator


trait Terminals { this: Parsers =>

  /**
    * A parser that always succeeds, consuming no input
    */
  case object Pass extends Parser[Unit] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[Unit] = success(ctx.success, (), ctx.input)
  }

  /**
    * A parser that always fails
    */
  case object Fail extends Parser[Nothing] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[Nothing] = failure(ctx.failure, "", ctx.input, List(ParseFrame(this, ctx.input)))
  }

  /**
    * Succeeds if at the start of  the input, consuming no input
    */
  case object Start extends Parser[Unit] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[Unit] =
      if (ctx.input.atStart) success(ctx.success, (), ctx.input)
      else failure(ctx.failure, "No input should have be consumed before Start", ctx.input, List(ParseFrame(this, ctx.input)))
  }

  /**
    * Succeed if at the end of the input, consuming no input
    */
  case object End extends Parser[Unit] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[Unit] =
      if (ctx.input.atEnd) success(ctx.success, (), ctx.input)
      else failure(ctx.failure,"All input should have be consumed before End", ctx.input, List(ParseFrame(this, ctx.input)))
  }

  case class Element(e: Elem) extends Parser[Elem] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[Elem] = accept(e)(ctx)
  }

  implicit def element(e: Elem) = Element(e)

  case object Index extends Parser[Int] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[Int] = success(ctx.success, ctx.input.offset, ctx.input)
  }

}
