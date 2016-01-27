package easyparse
package combinator

import easyparse.input.Reader
import scala.util.DynamicVariable
import scala.language.experimental.macros

abstract class  Parsers extends Combinators with Terminals with Transformers { outer =>
  type Elem

  type Input = Reader[Elem]

  var logged = false

  var traced = false

  lazy val lastFailure = new DynamicVariable[Option[MutableResult.Failure]](None)

  protected def output(s: String): Unit = println(s)

  def parse[T](p: Parser[T], in: Input): ParseResult[T]= p(new ParseContext(in, 0)).toResult

  def parseAll[T](p: Parser[T], in: Input): ParseResult[T] = {
    val np = LeftSequencer(p, End, false)
    lastFailure.withValue (None) {
      np(ParseContext(in, 0)) match {
        case s: MutableResult.Success[T] => s.toResult
        case f: MutableResult.Failure => lastFailure.value.getOrElse(f).toResult
      }
    }
  }

  implicit def enclosingFunctionName: Macros.FuncName = macro Macros.funcNameImpl

  /** A top-level parser that wrap the parser `p` into a lazy evaluated value
    *
    *  `rule(p, name)` return a `Rule` which name is `name`
    *
    *  @param  p   The element kind, used for error messages
    *  @param  name      A predicate that determines which elements match.
    *  @return
    */
  def P[T](p: => Parser[T])(implicit name: Macros.FuncName): Parser[T] = Rule(() => p, name.name)



  /** A parser matching input elements that satisfy a given predicate.
    *
    *  `elem(kind, p)` succeeds if the input starts with an element `e` for which `p(e)` is true.
    *
    *  @param  kind   The element kind, used for error messages
    *  @param  p      A predicate that determines which elements match.
    *  @return A parser for elements satisfying p(e) with expected value kind in error message
    */
  def elem(kind: String, p: Elem => Boolean) = acceptIf(p)(inEl => kind+" expected")

  /** A parser matching input elements that satisfy a given predicate.
    *
    *  `acceptIf(p)(el => "Unexpected "+el)` succeeds if the input starts with an element `e` for which `p(e)` is true.
    *
    *  @param  err    A function from the received element into an error message.
    *  @param  p      A predicate that determines which elements match.
    *  @return        A parser for elements satisfying p(e).
    */
  def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = new Parser[Elem] {
      def apply(in: ParseContext): MutableResult.MutableResult[Elem] = {
        if (in.input.atEnd) failure(in.failure, "at end of input", in.input, List(ParseFrame(this, in.input)))
        else if (p(in.input.first)) success(in.success, in.input.first, in.input.rest)
        else failure(in.failure, err(in.input.first), in.input, List(ParseFrame(this, in.input)))
      }
  }

  /** A parser that matches only the given element `e`.
    *
    *  The method is implicit so that elements can automatically be lifted to their parsers.
    *  For example, when parsing `Token`s, `Identifier("new")` (which is a `Token`) can be used directly,
    *  instead of first creating a `Parser` using `accept(Identifier("new"))`.
    *
    *  @param e the `Elem` that must be the next piece of input for the returned parser to succeed
    *  @return a `tParser` that succeeds if `e` is the next available input.
    */
  def accept(e: Elem): Parser[Elem] = acceptIf(_ == e)(e.toString + "expected but" + _ + "found")



  sealed abstract class ParseResult[+T] {

    /** Functional composition of ParseResults.
      *
      * @param f the function to be lifted over this result
      * @return `f` applied to the result of this `ParseResult`, packaged up as a new `ParseResult`
      */
    def map[U](f: T => U): ParseResult[U]

    def mapPartial[U](f: PartialFunction[T, U], unmatch: T => String): ParseResult[U]

    def get: T

    def getOrElse[B >: T](default: => B): B =
      if (isEmpty) default else get

    def isEmpty = !successful

    val successful: Boolean
  }

  case class Success[T](result: T, next: Input) extends ParseResult[T] {
    override def toString = s"[${next.pos}] parsed: $result"

    def map[U](f: T => U): ParseResult[U] = Success(f(result), next)

    def mapPartial[U](f: PartialFunction[T, U], unmatch: T => String): ParseResult[U] = {
      if (f.isDefinedAt(result)) Success(f(result), next) else Failure(unmatch(result), next, Nil)
    }

    def get: T = result

    val successful = true
  }

  case class Failure(msg: String,
                     next: Input,
                     traceStack: List[ParseFrame]) extends ParseResult[Nothing] {

    val successful = false

    override def toString = s"[${next.pos}] failed: $msg\n" + next.pos.draw

    def map[U](f: Nothing => U): ParseResult[U] = this

    def mapPartial[U](f: PartialFunction[Nothing, U], unmatch: Nothing => String): ParseResult[U] = this

    def get = throw new Exception("No result when parsing failed")

    def traceInfo: String = traceStack.map {
      frame =>
        s"[${frame.input.pos}] using parser: ${frame.parser}"
    }.mkString("\n")

  }

  case class ParseFrame(parser: Parser[_], input: Input)

  /**
    * parse context that contains input and other infomation
    * which need to be transmit through parsing process
    */

  case class ParseContext(input: Input, var logDepth: Int = 0) {

    val failure = MutableResult.Failure("", input, Nil)
    val success = MutableResult.Success(null, input)
  }

  trait ParserApi[+T] { this: Parser[T] =>

    /**
      * Repeats this parser 0 or more times
      */
    def rep(sep: => Parser[_] = Pass, min: Int = 0,  max: Int = Int.MaxValue): Parser[List[T]] = new Repeat(this, min, max, sep) with ParserLogger[List[T]]

    /** Returns a parser that repeatedly parses what this parser parses.
      *
      *  @return Parser[List[Parser[T] ] ]
      */
    def * = rep()

    /** Returns a parser that repeatedly (at least once) parses what this parser parses.
      *
      *  @return rep1(this)
      */
    def + = rep(Pass, 1)

    /** A parser combinator for alternative composition.
      *
      *  `p | q` succeeds if `p` succeeds or `q` succeeds.
      *   Note that `q` is only tried if `p`s failure is non-fatal (i.e., back-tracking is allowed).
      *
      * @param q a parser that will be executed if `p` (this parser) fails (and allows back-tracking)
      * @return a `Parser` that returns the result of the first parser to succeed (out of `p` and `q`)
      *         The resulting parser succeeds if (and only if)
      *         - `p` succeeds, ''or''
      *         - if `p` fails allowing back-tracking and `q` succeeds.
      */
    def | [U >: T](q: => Parser[U]): Parser[U] = new Either(Either.flatten(Vector(this, q)):_*) with ParserLogger[T]

    /** A parser combinator for alternative with longest match composition.
      *
      *  `p ||| q` succeeds if `p` succeeds or `q` succeeds.
      *  If `p` and `q` both succeed, the parser that consumed the most characters accepts.
      *
      * @param q a parser that accepts if p consumes less characters. -- evaluated at most once, and only when necessary
      * @return a `Parser` that returns the result of the parser consuming the most characters (out of `p` and `q`).
      */
    def ||| [U >: T](q: => Parser[U]): Parser[U] = new GreedyEither(GreedyEither.flatten(Vector(this, q)): _*) with ParserLogger[T]

    /** A parser combinator for sequential composition.
      *
      * `p ~ q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *
      * @param q a parser that will be executed after `p` (this parser)
      *          succeeds -- evaluated at most once, and only when necessary.
      * @return a `Parser` that -- on success -- returns a `~` (like a `Pair`,
      *         but easier to pattern match on) that contains the result of `p` and
      *         that of `q`. The resulting parser fails if either `p` or `q` fails.
      */
    def ~ [U](q: => Parser[U]): Parser[~[T, U]] = new Sequencer(this, q, false) with ParserLogger[~[T, U]]

    /** A parser combinator for sequential composition which keeps only the right result.
      *
      * `p ~> q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *
      * @param q a parser that will be executed after `p` (this parser)
      *        succeeds -- evaluated at most once, and only when necessary.
      * @return a `Parser` that -- on success -- returns the result of `q`.
      */
    def ~> [U](q: => Parser[U]): Parser[U] = RightSequencer(this, q, false)

    /** A parser combinator for sequential composition which keeps only the left result.
      *
      *  `p <~ q` succeeds if `p` succeeds and `q` succeeds on the input
      *           left over by `p`.
      *
      * @note <~ has lower operator precedence than ~ or ~>.
      *
      * @param q a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when necessary
      * @return a `Parser` that -- on success -- returns the result of `p`.
      */
    def <~ [U](q: => Parser[U]): Parser[T] = LeftSequencer(this, q, false)

    /** A parser combinator for non-back-tracking sequential composition.
      *
      *  `p ~! q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *   In case of failure, no back-tracking is performed (in an earlier parser produced by the `|` combinator).
      *
      * @param p a parser that will be executed after `p` (this parser) succeeds
      * @return a `Parser` that -- on success -- returns a `~` (like a Pair, but easier to pattern match on)
      *         that contains the result of `p` and that of `q`.
      *         The resulting parser fails if either `p` or `q` fails, this failure is fatal.
      */
    def ~! [U](p: => Parser[U]): Parser[~[T, U]] = new Sequencer(this, p, true) with ParserLogger[~[T, U]]

    /** A parser combinator for function application.
      *
      *  `p ^^ f` succeeds if `p` succeeds; it returns `f` applied to the result of `p`.
      *
      * @param f a function that will be applied to this parser's result (see `map` in `ParseResult`).
      * @return a parser that has the same behaviour as the current parser, but whose result is
      *         transformed by `f`.
      */
    def ^^ [U](f: T => U): Parser[U] = new Mapper(this, f) with ParserLogger[U]

    /**
      * Returns a parser that optionally parses what this parser parses.
      */
    def ? : Parser[Option[T]] = new Optional(this) with ParserLogger[Option[T]]

    /**
      * Reverse the result of this parser
      */
    def unary_! : Parser[Unit] = new Not(this) with ParserLogger[Unit]

    /**
      * applies the supplied predicate to the current parser succeeding on true failing on false
      */
    def filter(predicate: T => Boolean): Parser[T] = new Filter(this, predicate) with ParserLogger[T]
  }

  /**
    * a immutable parser which parse an input of type [U] into a result of  type [T]
    * or a failure with some traced information
    */

  abstract class Parser[+T] extends ParserApi[T] {

    def apply(in: ParseContext): MutableResult.MutableResult[T]

    def success[T](init: MutableResult.Success[_],
                   value: T,
                   next: Input,
                   cut: Boolean = false) = {

      val s = init.asInstanceOf[MutableResult.Success[T]]
      s.result = value
      s.next = next
      s.cut = cut
      s
    }

    def failure(init: MutableResult.Failure,
                msg: String,
                next: Input,
                traceStack: List[ParseFrame],
                cut: Boolean = false) = {
      val ret = if (lastFailure.value.isEmpty) {
        lastFailure.value = Some(init)
        init
      } else {
        if (lastFailure.value forall (_ == init))
          MutableResult.Failure(msg, next, Nil)
        else init
      }

      if (msg != null) ret.msg = msg
      ret.next = next
      if (traced) ret.traceStack = traceStack
      ret.cut = cut

      if (lastFailure.value.forall(v => !(next.pos < v.next.pos)))
        lastFailure.value = Some(ret)
      ret
    }

    def trace(init: MutableResult.Failure,
              frame: ParseFrame,
              cut: Boolean = false) = {

      if (lastFailure.value.isEmpty) lastFailure.value = Some(init)
      if (traced && init == lastFailure.value.get)
        init.traceStack = frame :: init.traceStack
      init.cut |= cut
      init
    }


  }

  object Parser {

    def apply[T](f: ParseContext => MutableResult.MutableResult[T]) = new Parser[T] { def apply(in: ParseContext) = f(in) }

  }

  trait ParserLogger[+T] extends Parser[T] {
    abstract override def apply(ctx: ParseContext): MutableResult.MutableResult[T] = {
      if (logged) {
        val indent = " " * ctx.logDepth
        val enterInfo = s"${indent}[${ctx.input.pos}] Trying to parse using parser: ${this}"
        output(enterInfo)
        val r = super.apply(ctx)
        val leaveInfo =  s"${indent}[${ctx.input.pos}] Leaving parser with result: $r"
        output(leaveInfo)
        r
      } else {
        super.apply(ctx)
      }
    }
  }

  /**
    * mutable version of ParseResult
    */
  object MutableResult {

    trait MutableResult[+T] {

      var cut: Boolean
      def toResult: ParseResult[T]
    }

    case class Success[T](var result: T,
                           var next: Input,
                           var cut: Boolean = false) extends MutableResult[T] {

      override def toString = s"Success($result)"

      def toResult = outer.Success(result, next)

      val successful = true
    }

    case class Failure(var msg: String,
                       var next: Input,
                       var traceStack: List[ParseFrame],
                       var cut: Boolean = false) extends MutableResult[Nothing] {

      val successful = false

      override def toString = s"Failure($msg)"

      def toResult = outer.Failure(msg, next, traceStack)

    }

  }

}