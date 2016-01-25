package scame

import scala.util.control.Exception

sealed abstract class SchemeException extends Exception

case class NumArgsException(expected: Int, found: List[SchemeVal]) extends SchemeException {
  override def toString = "Expected " + expected.toString + " args: found values " + found.mkString(" ")
} 

case class TypeMismatchException(expected: String, found: SchemeVal) extends SchemeException {
  override def toString = "Invalid type: expected " + expected + ", found " + found.toString
}

case class ParseException(msg: String) extends SchemeException {
  override def toString = "Syntax error: " + msg
}

case class BadSpecialFormException(msg: String, form: SchemeVal) extends SchemeException {
  override def toString = msg + ": " + form
}

case class NotFunctionException(msg: String, func: String) extends SchemeException {
  override def toString = msg + ": " + func
}

case class UnboundVarException(msg: String, varName: String ) extends SchemeException {
  override def toString = msg + ": " + varName
}

case class DefaultException(msg: String ) extends SchemeException {
  override def toString = msg
}