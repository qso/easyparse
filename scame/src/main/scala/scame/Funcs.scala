package scame

import util.Try
import java.io._
import scala.io.BufferedSource

object PrimitiveFuncs {
  
  def primitives: Map[String, List[SchemeVal] ⇒ Try[SchemeVal]] = Map (
    "+" → numericBinop(((x:Int, y: Int) ⇒ x + y))_ ,
    "-" → numericBinop((_: Int) - (_: Int))_ ,
    "*" → numericBinop((_: Int) * (_: Int))_ ,
    "/" → numericBinop((_: Int) / (_: Int))_ ,
    "mod" → numericBinop((_: Int) % (_: Int))_ ,
    "quotient" → numericBinop((_: Int) - (_: Int))_ ,
    "remainder" → numericBinop((_: Int) % (_: Int))_ ,
    "=" → numBoolBinop((_: Int) == (_: Int)) ,
    "<" → numBoolBinop((_: Int) < (_: Int)) ,
    ">" → numBoolBinop((_: Int) > (_: Int)) ,
    "/=" → numBoolBinop((_: Int) != (_: Int)) ,
    "<=" → numBoolBinop((_: Int) <= (_: Int)) ,
    ">=" → numBoolBinop((_: Int) >= (_: Int)) ,
    "&&"→ boolBoolBinop((_: Boolean) && (_: Boolean)) ,
    "||"→ boolBoolBinop((_: Boolean) || (_: Boolean)) ,
    "car" → car ,
    "cdr" → cdr ,
    "cons" → cons , 
    "eq?" → eqv ,
    "eqv?" → eqv ,
    "open-input-file" → makeReadPort ,
    "open-output-file" → makeWritePort ,
    "close-input-file" → closePort ,
    "close-output-file" → closePort ,
    "read" → readProc ,
    "write" → writeProc ,
    "read-contents" → readContents ,
    "read-all" → readAll
  )
  
  def makeEnv: Env = (new Env(null)).bindVars(primitives.mapValues(SchemePrimitive(_)))
  
  def apply(func: String)(args: List[SchemeVal]): Try[SchemeVal] = {
    val f = primitives.get(func)
    if (f.isEmpty) Try(throw new NotFunctionException("Unrecognized primivitive function args", func))
    else (f.get)(args)
  }
  
  def sequence[T](seq: List[Try[T]]): Try[List[T]] =  seq match {
    case Nil ⇒ Try(Nil)
    case h :: t ⇒ h flatMap (hh ⇒ sequence(t) map (hh :: _))
  }
  
  private def numericBinop(op: (Int , Int) ⇒ Int)(param: List[SchemeVal]): Try[SchemeVal] = param match {
    case singleVal @ List(_) ⇒ Try(throw new NumArgsException(2, singleVal))
    case _ ⇒ sequence(param map unpackNum) map (l ⇒ SchemeInt(l reduceLeft op))
  }
  
  private def boolBinop[T](unpacker: SchemeVal ⇒ Try[T])(op: (T, T) ⇒ Boolean)(param: List[SchemeVal]): Try[SchemeVal] = {
    if (param.length != 2) Try(throw new NumArgsException(2, param))
    else for (a ← unpacker(param(0)); b ← unpacker(param(1))) yield SchemeBool(op(a, b))
  }
  
  private def numBoolBinop = boolBinop(unpackNum)_
  private def strBoolBinop = boolBinop(unpackString)_
  private def boolBoolBinop = boolBinop(unpackBool)_
  
  private def unpackNum(param: SchemeVal): Try[Int] = Try {
    param match {
      case SchemeInt(n) ⇒ n
      case all@SchemeString(s) ⇒ Try(s.toInt).getOrElse(throw new TypeMismatchException("number", all))
      case SchemeList(List(n)) ⇒ unpackNum(n).get
      case notNum ⇒ throw new TypeMismatchException("number", notNum)
    }
  }
  private def unpackString(param: SchemeVal): Try[String] = Try {
    param match {
      case SchemeInt(n) ⇒ n.toString
      case SchemeString(s) ⇒ s
      case SchemeBool(b) ⇒ b.toString
      case notString ⇒ throw new TypeMismatchException("string", notString)
    }
  }
  
  private def unpackBool(param: SchemeVal): Try[Boolean] = Try {
    param match {
      case SchemeBool(b) ⇒ b
      case notBool ⇒ throw new TypeMismatchException("boolean", notBool)
    }
  }
    
  private def car(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(SchemeList(x :: xs)) ⇒Try(x)
    case List(SchemeDottedList((x :: xs), _)) ⇒ Try(x)
    case List(badArg) ⇒ Try(throw new TypeMismatchException("pair", badArg))
    case badArgList ⇒ Try(throw new NumArgsException(1, badArgList))
  }
  
  private def cdr(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(SchemeList(x :: xs)) ⇒Try(SchemeList(xs))
    case List(SchemeDottedList((_ :: xs), last)) ⇒ Try(SchemeDottedList(xs, last))
    case List(SchemeDottedList(List(xs), last)) ⇒ Try(last)
    case List(badArg) ⇒ Try(throw new TypeMismatchException("pair", badArg))
    case badArgList ⇒ Try(throw new NumArgsException(1, badArgList))
  }
  
  private def cons(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(x, SchemeList(Nil)) ⇒Try(SchemeList(List(x)))
    case List(x, SchemeList(xs)) ⇒Try(SchemeList(x :: xs))
    case List(x, SchemeDottedList(xs, last)) ⇒ Try(SchemeDottedList(x :: xs, last))
    case List(x, xs) ⇒ Try(SchemeDottedList(List(x), xs))
    case badArgList ⇒ Try(throw new NumArgsException(2, badArgList))
  }
  
  private def eqv(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(SchemeBool(arg1), SchemeBool(arg2)) ⇒ Try(SchemeBool(arg1 == arg2))
    case List(SchemeInt(arg1), SchemeInt(arg2)) ⇒ Try(SchemeBool(arg1 == arg2))
    case List(SchemeString(s1), SchemeString(s2)) ⇒ Try(SchemeBool(s1 == s2))
    case List(SchemeAtom(a1), SchemeAtom(a2)) ⇒ Try(SchemeBool(a1 == a2))
    case List(SchemeDottedList(xs, x), SchemeDottedList(ys, y)) ⇒ Try(SchemeBool(xs == ys && x == y))
    case List(SchemeList(xs), SchemeList(ys)) ⇒ Try(SchemeBool(xs == ys))
    case List(_, _) ⇒ Try(SchemeBool(false))
    case badArgList ⇒ Try(throw new NumArgsException(2, badArgList))
  }
  
  private def makeReadPort(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(SchemeString(filename)) ⇒ Try(SchemePort(Left(new FileReader(filename))))
    case List(badArg) ⇒ Try(throw new TypeMismatchException("string", badArg))
    case badArgList ⇒ Try(throw new NumArgsException(1, badArgList))
  }
  
  private def makeWritePort(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(SchemeString(filename)) ⇒ Try(SchemePort(Right(new FileWriter(filename))))
    case List(badArg) ⇒ Try(throw new TypeMismatchException("string", badArg))
    case badArgList ⇒ Try(throw new NumArgsException(1, badArgList))
  }
  
  private def closePort(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(SchemePort(port)) ⇒ if (port.isLeft) port.left.get.close() else port.right.get.close(); Try(SchemeBool(true))
    case _  ⇒ Try(SchemeBool(false))
  }
  
  private def readProc(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case Nil ⇒ readProc(List(SchemePort(Left(new FileReader(FileDescriptor.in)))))
    case List(SchemePort(port)) ⇒ ScameParser.parseExpr((new BufferedReader(port.left.get).readLine()))
    case List(badArg) ⇒ Try(throw new TypeMismatchException("port", badArg))
    case badArgList ⇒ Try(throw new NumArgsException(1, badArgList))
  }
  
  private def writeProc(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(obj) ⇒ writeProc(List(obj, SchemePort(Right(new FileWriter(FileDescriptor.out)))))
    case List(obj, SchemePort(port)) ⇒ port.right.get.write(obj.toString); Try(SchemeBool(true))
    case List(badArg) ⇒ Try(throw new TypeMismatchException("port", badArg))
    case badArgList ⇒ Try(throw new NumArgsException(1, badArgList))
  }
  
  private def readContents(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(SchemeString(filename)) ⇒ Try(SchemeString(io.Source.fromFile(filename).mkString("\n")))
    case List(badArg) ⇒ Try(throw new TypeMismatchException("string", badArg))
    case badArgList ⇒ Try(throw new NumArgsException(1, badArgList))
  }
  
  def loadSchemeFile(filename: String): Try[List[SchemeVal]] = {
    ScameParser.parseExprList(new FileReader(filename))
  }
  
  
  private def readAll(args: List[SchemeVal]): Try[SchemeVal] = args match {
    case List(SchemeString(filename)) ⇒ for (li ← loadSchemeFile(filename)) yield SchemeList(li)
    case List(badArg) ⇒ Try(throw new TypeMismatchException("string", badArg))
    case badArgList ⇒ Try(throw new NumArgsException(1, badArgList))
  }
  
}