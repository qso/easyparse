import easyparse.combinator.CharSeqParsers


class TestParsers extends CharSeqParsers {

  traced = true


  def rule = "haha" ~ ("gege" | "xixi" | "coco")

  def rule2 = "ha".rep(sep = ",") | "123" ^^ {_.toInt}

  def rule3 = Either("12345", "12360")
  def rule4 = rule("adsf", "rule4")

}
val ps = new TestParsers
//ps.parse(ps.rule, "hahatexixi") match {
//  case f: ps.Failure => f.traceInfo
//  case _ =>  "success"
//}
//ps.rule2
//ps.parseAll(ps.rule2, "ha,ha,ha,ha,ha,haha")
ps.parseAll(ps.rule3, "12367")
//ps.parseAll(ps.stringLiteral, "asas")
println(ps.parseAll(ps.rule4, "asdf"))
