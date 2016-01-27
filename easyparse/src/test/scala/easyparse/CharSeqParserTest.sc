import easyparse.Api._
import easyparse.combinator.RegexParsers
class TestParsers extends RegexParsers {

  traced = true

  override def skipWhiteSpace = false

  def rule = P("haha" ~ ("gege" | "xixi" | "coco"))

  def rule2 = P("ha".rep(whiteSpace) | "123" ^^ {_.toInt})

  def rule3 = P(Either("12345", "12360"))
  def rule4 = P("adsf")
}
val ps = new TestParsers
//ps.parse(ps.rule, "hahatexixi") match {
//  case f: ps.Failure => f.traceInfo
//  case _ =>  "success"
//}
//ps.rule2
ps.parse(ps.rule2, "ha ha ha ha      ha     ")
ps.parseAll(ps.rule3, "12367")
//ps.parseAll(ps.stringLiteral, "asas")
println(ps.parseAll(ps.rule4, "asdf"))
//import easyparse.combinator.A
//import scala.language.experimental.macros
////import easyparse.Macros._
//
//
//class B extends A {
//  def hello2 = hello("beauty")
//}
//class C extends B {
//  def hello3 = hello("shuaige")
//}
//val c = new C
//c.hello3
//
//val b = new B
//b.hello2