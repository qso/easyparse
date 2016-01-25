package easyparse
package combinator


import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by qso on 15-12-15.
  */
trait Combinators { this: Parsers =>


  /**
    * A top-level, named parser. Lazily evaluates the wrapped parser
    * [[p]] only when `parse` is called to allow for circular
    * dependencies between parsers.
    */
  case class Rule[+T](p: () => Parser[T], name: String) extends Parser[T]{
    private[this] lazy val pCached = p()

    def apply(ctx: ParseContext): MutableResult.MutableResult[T] = {

      pCached.apply(ctx) match{
        case f: MutableResult.Failure => f
        case s: MutableResult.Success[T] => s
      }
    }

    override def toString = s"Rule($name)"

  }

  case class ~[+T1, +T2](_1: T1, _2: T2) {
    override def toString = s"(${_1} ~ ${_2})"
  }

  case class Sequencer[+T1, +T2](p1: Parser[T1], p2: Parser[T2], cut: Boolean) extends Parser[~[T1, T2]] {

    def apply(ctx: ParseContext): MutableResult.MutableResult[~[T1, T2]] = {
      ctx.logDepth += 1
      val ret = p1(ctx) match {
        case fa: MutableResult.Failure =>
          trace(fa, ParseFrame(this, ctx.input), cut)
        case su: MutableResult.Success[T1] => p2(ParseContext(su.next, ctx.logDepth)) match {
          case fa2: MutableResult.Failure =>
            trace(fa2, ParseFrame(this, ctx.input), cut)
          case su2: MutableResult.Success[T2] => success(ctx.success, new ~(su.result, su2.result), su2.next, su.cut | su2.cut)
        }
      }
      ctx.logDepth -= 1
      ret
    }

    override def toString = p1.toString + " ~ " + p2.toString
  }

  case class LeftSequencer[+T1, +T2](p1: Parser[T1], p2: Parser[T2], cut: Boolean) extends Parser[T1] {

    def apply(ctx: ParseContext): MutableResult.MutableResult[T1] = {
      ctx.logDepth += 1
      val ret = p1(ctx) match {
        case fa: MutableResult.Failure =>
          trace(fa, ParseFrame(this, ctx.input), cut)
        case su: MutableResult.Success[T1] => p2(ParseContext(su.next, ctx.logDepth)) match {
          case fa2: MutableResult.Failure =>
            trace(fa2, ParseFrame(this, ctx.input), cut)
          case su2: MutableResult.Success[T2] => success(ctx.success, su.result, su2.next, su.cut | su2.cut)
        }
      }
      ctx.logDepth -= 1
      ret
    }

    override def toString = p1.toString + " <~ " + p2.toString
  }

  case class RightSequencer[+T1, +T2](p1: Parser[T1], p2: Parser[T2], cut: Boolean) extends Parser[T2] {

    def apply(ctx: ParseContext): MutableResult.MutableResult[T2] = {
      ctx.logDepth += 1
      val ret = p1(ctx) match {
        case fa: MutableResult.Failure =>
          trace(fa, ParseFrame(this, ctx.input), cut)
        case su: MutableResult.Success[T1] => p2(ParseContext(su.next, ctx.logDepth)) match {
          case fa2: MutableResult.Failure =>
            trace(fa2, ParseFrame(this, ctx.input), cut)
          case su2: MutableResult.Success[T2] => success(ctx.success, su2.result, su2.next, su.cut | su2.cut)
        }
      }
      ctx.logDepth -= 1
      ret
    }

    override def toString = p1.toString + " ~> " + p2.toString
  }

  object Either {
    def flatten[T](p: Vector[Parser[T]]): Vector[Parser[T]] = p flatMap {
      case Either(ps@_*) => ps
      case  other => Vector(other)
    }
  }

  case class Either[T](ps: Parser[T]*) extends Parser[T]  {
    private[this] val ps0 = ps.toArray
    private[this] val n = ps0.length

    def apply(ctx: ParseContext): MutableResult.MutableResult[T] = {
      @tailrec def rec(index: Int): MutableResult.MutableResult[T] = {
        if (index >= n) failure(ctx.failure, "None the parsers match the input", ctx.input, List(ParseFrame(this, ctx.input)))
        else ps0(index)(ctx) match {
          case s: MutableResult.Success[T] => s
          case f: MutableResult.Failure =>
            if (f.cut) trace(f, ParseFrame(this, ctx.input))
            else rec(index + 1)
        }
      }
      ctx.logDepth += 1
      val ret = rec(0)
      ctx.logDepth -= 1
      ret
    }

    override def toString = s"${ps.mkString(" | ")}"
  }

  object GreedyEither {
    def flatten[T](p: Vector[Parser[T]]): Vector[Parser[T]] = p flatMap {
      case GreedyEither(ps@_*) => ps
      case  other => Vector(other)
    }
  }

  case class GreedyEither[T](ps: Parser[T]*) extends Parser[T]  {

    def apply(ctx: ParseContext): MutableResult.MutableResult[T] = {
      val ps0 = ps.toArray
      var maxSuc: MutableResult.Success[T] = null
      val n = ps0.length
      @tailrec def rec(index: Int): MutableResult.MutableResult[T] = {
        if (index >= n) {
          if (maxSuc == null) failure(ctx.failure, "None the parsers match the input", ctx.input, List(ParseFrame(this, ctx.input)))
          else maxSuc
        }
        else ps0(index)(ctx) match {
          case s: MutableResult.Success[T] =>
            if (maxSuc == null) {
              maxSuc = s
              rec(index + 1)
            } else {
              if (maxSuc.next.pos < s.next.pos)
                maxSuc = s
              rec(index + 1)
            }
          case f: MutableResult.Failure =>
            if (f.cut) trace(f, ParseFrame(this, ctx.input))
            else rec(index + 1)
        }
      }
      ctx.logDepth += 1
      val ret = rec(0)
      ctx.logDepth -= 1
      ret
    }

    override def toString = s"${ps.mkString(" ||| ")}"
  }

  case class Repeat[T](p: Parser[T], min: Int, max: Int, delimiter: Parser[_]) extends Parser[List[T]] {

    override def apply(ctx: ParseContext): MutableResult.MutableResult[List[T]] = {
      val parserList = new ListBuffer[T]
      @tailrec def rec(count: Int, context: ParseContext, cut: Boolean): MutableResult.MutableResult[List[T]] = {
        delimiter(context) match {
          case f: MutableResult.Failure =>
            if (count >= min) success(ctx.success, parserList.toList, context.input, cut)
            else failure(ctx.failure, s"Can't match enough $p (min = $min)", ctx.input, List(ParseFrame(this, ctx.input)))
          case s: MutableResult.Success[_] =>
            p(ParseContext(s.next, context.logDepth)) match {
              case f2: MutableResult.Failure =>
                if (f2.cut | s.cut) trace(f2, ParseFrame(this, context.input), true)
                else if (count >= min) success(ctx.success, parserList.toList, context.input)
                else failure(ctx.failure, s"Can't match enough $p (min = $min)", ctx.input, List(ParseFrame(this, ctx.input)))
              case s2: MutableResult.Success[T] =>
                parserList += s2.result
                if (count + 1 < max) rec(count + 1, ParseContext(s2.next, context.logDepth), cut | s.cut | s2.cut)
                else success(ctx.success, parserList.toList, s2.next, cut | s.cut | s2.cut)
            }
        }
      }
      ctx.logDepth += 1
      val ret = p(ctx) match {
        case f: MutableResult.Failure => trace(f, ParseFrame(this, ctx.input))
        case s: MutableResult.Success[T] =>
          parserList += s.result
          rec(1, ParseContext(s.next, ctx.logDepth), s.cut)
      }
      ctx.logDepth -= 1
      ret
    }

    override def toString = s"Repeat($p${if(delimiter != Pass) ", sep="+delimiter})"
  }

  case class Not[T](p: Parser[T]) extends Parser[Unit] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[Unit] = {
      ctx.logDepth += 1
      val ret = p(ctx) match {
        case s: MutableResult.Success[T] => failure(ctx.failure, s"Parser $p should not be matched", ctx.input, List(ParseFrame(this, ctx.input)))
        case f: MutableResult.Failure => success(ctx.success, (), ctx.input)
      }
      ctx.logDepth -= 1
      ret
    }
  }

  case class Optional[T](p: Parser[T]) extends Parser[Option[T]] {
    def apply(ctx: ParseContext): MutableResult.MutableResult[Option[T]] = {
      ctx.logDepth += 1
      val ret = p(ctx) match {
        case s: MutableResult.Success[T] => success(s, Some(s.result), s.next, s.cut)
        case f: MutableResult.Failure => success(ctx.success, None, ctx.input)
      }
      ctx.logDepth -= 1
      ret
    }
  }

  /**
    * Wraps another parser, succeeding/failing identically
    * but consuming no input
    */
  case class Lookahead(p: Parser[_]) extends Parser[Unit]{
    def apply(ctx: ParseContext): MutableResult.MutableResult[Unit] = {
      ctx.logDepth += 1
      val ret = p(ctx) match{
        case s: MutableResult.Success[_] => success(ctx.success, (), ctx.input)
        case f: MutableResult.Failure =>
          f.cut = false
          trace(f, ParseFrame(this, ctx.input))
      }
      ctx.logDepth -= 1
      ret
    }
  }

}
