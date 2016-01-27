package easyparse


import easyparse.Api._
import scala.collection.immutable.Map
import scala.language.experimental.macros
import org.scalatest._
/**
  * Created by qso on 16-1-14.
  */
//
//class JsonSpec extends FeatureSpec with GivenWhenThen {
//
//  info("A complete JSON parser that parse JSON into scala's data structures")
//  feature("JSON Parsing Result Validation") {
//    scenario("An address book should completely be parsed") {
//      val jp = new JSON1
//      val book = """{
//                   "address book": {
//                   "name": "John Smith",
//                   "address": {
//                   "street": "10 Market Street",
//                   "city" : "San Francisco, CA",
//                   "zip" : 94111
//                   },
//                   "phone numbers": [
//                   "408 338-4238",
//                   "408 111-6892"
//                   ]
//                   }
//                   }"""
//      val rightRet = Map("address book" -> Map("name" -> "John Smith", "address" -> Map("street" -> "10 Market Street", "city" -> "San Francisco, CA", "zip" -> 94111.0), "phone numbers" -> List("408 338-4238", "408 111-6892")))
//      Given("an address book below\n" + book)
//      assert(jp.parseAll(jp.value, book).get == rightRet)
//    }
//  }
//}
//
//class JSON1 extends RegexParsers {
//  def stringLiteral: Parser[String] =
//    P(("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+"""+"\"").r ^^ (_.drop(1).dropRight(1)))
//  def floatingPointNumber: Parser[String] =
//    P("""-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r)
//  def obj: Parser[Map[String, Any]] =
//    P(("{" ~  member.rep(sep = ",")  ~ "}" ) ^^ {case "{" ~ ms ~ "}" => Map() ++ ms})
//  def arr: Parser[List[Any]] =
//    P(("[" ~  value.rep(sep = ",")  ~ "]") ^^ {case "[" ~ v ~ "]" => v})
//  def member: Parser[(String, Any)] =
//    P((stringLiteral ~ ":" ~ value) ^^
//      { case name ~ ":" ~ value => (name, value) })
//  def value: Parser[Any] = P(
//    obj
//      | arr
//      | stringLiteral
//      | (floatingPointNumber ^^ (_.toDouble))
//      | ("null" ^^ (x => null))
//      | ("true" ^^ (x => true))
//      | ("false" ^^ (x => false))
//  )
//}