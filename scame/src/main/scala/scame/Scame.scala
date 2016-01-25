package scame

import easyparse.combinator._
import util.Try
import scala._


abstract class SchemeVal
case class SchemeAtom(name: String) extends SchemeVal {
  override def toString = name
}

case class  SchemeList(contents: List[SchemeVal]) extends SchemeVal {
  override def toString = "(" + contents.map(_.toString).mkString(" ") + ")"
}

case class SchemeDottedList(init: List[SchemeVal], last: SchemeVal) extends SchemeVal {
  override def toString =  "(" + init.mkString(" ") + " . " + last.toString + ")"
}

case class SchemeInt(num: Int) extends SchemeVal {
  override def toString = num.toString
}

case class SchemeDouble(num: Double) extends SchemeVal {
  override def toString = num.toString
}

case class SchemeString(contents: String) extends SchemeVal {
  override def toString = "\"" + contents + "\""
}

case class SchemeBool(value: Boolean) extends SchemeVal {
  override def toString = value match {
    case true ⇒ "#t"
    case false ⇒ "#f"
  }
}

case class SchemePrimitive(func: List[SchemeVal] ⇒ Try[SchemeVal]) extends SchemeVal {
  override def toString = "<primitive>"
}

case class SchemeFunc(params: List[String], vararg: Option[String], body: List[SchemeVal], closure: Env) extends SchemeVal {
  override def toString = "(lambda (" + params.mkString(" ") + 
    (vararg match {case None ⇒ " "; case Some(arg) ⇒ " . " + arg}) + ") ...)"
}

case class SchemePort(handler: Either[java.io.Reader, java.io.Writer]) extends SchemeVal {
  override def toString = "<IO port>"
}



object ScameParser extends RegexParsers {
  
  override def skipWhiteSpace = false

  def wholeNumber: Parser[String] =
    """-?\d+""".r
  
  def atomLiteral: Parser[SchemeVal] = (letter | symbol) ~ (letter | digit | symbol).+ ^^ {
    case h ~ t ⇒ 
      val text = h.toString + t.mkString
      text match {
        case "#t" ⇒ SchemeBool(true)
        case "#f" ⇒ SchemeBool(false)
        case _ ⇒ SchemeAtom(text)

      }
  }
  
  def listLiteral: Parser[SchemeVal] = expr.rep(whiteSpace) ^^ (SchemeList(_))
  
  def dottedListLiteral: Parser[SchemeVal] = expr.rep(whiteSpace) ~ whiteSpace ~ '.' ~ whiteSpace ~ expr ^^ {
    case init ~ _ ~ '.' ~ _ ~ last ⇒ SchemeDottedList(init, last)
  }
  
  def quoteLiteral: Parser[SchemeVal] = '\'' ~> expr ^^ {x: SchemeVal ⇒ SchemeList(List(SchemeAtom("quote"), x))}
  
  def intLiteral: Parser[SchemeVal] = wholeNumber ^^ {x: String ⇒ SchemeInt(x.toInt)}
  
//  def doubleLiteral: Parser[SchemeVal] = floatingPointNumber ^^ {x: String ⇒ SchemeDouble(x.toDouble)}
  
  def strLiteral: Parser[SchemeVal] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+"""+"\"").r ^^ (ret => SchemeString(ret.drop(1).dropRight(1)))
  
  def expr: Parser[SchemeVal] = 
    ( atomLiteral 
    | quoteLiteral
    | strLiteral
//    | doubleLiteral
    | intLiteral
    | '(' ~> ( listLiteral | dottedListLiteral) <~ ')' )
    
  def exprList: Parser[List[SchemeVal]] = rule(whiteSpace.? ~> expr.rep(whiteSpace) <~ whiteSpace.?, "Expression List")
    
  
  def symbol = elem("symbol", "!$%&|*+-/:<=?>@^_~#".contains(_))
  def letter = elem("letter", _.isLetter)
  def digit = elem("digit", _.isDigit)
  
  def parseExpr(s: java.lang.CharSequence): Try[SchemeVal] = {
    parseAll(expr, s) match {
      case Success(value, _) ⇒ Try(value)
      case failure ⇒ Try(throw new ParseException(failure.toString))
    }
  }
  
  def parseExpr(s: java.io.Reader): Try[SchemeVal] = {
    parseAll(expr, s) match {
      case Success(value, _) ⇒ Try(value)
      case failure ⇒ Try(throw new ParseException(failure.toString))
    }
  }
  
  def parseExprList(s: java.lang.CharSequence): Try[List[SchemeVal]] = {
    parseAll(exprList, s) match {
      case Success(value, _) ⇒ Try(value)
      case failure ⇒ Try(throw new ParseException(failure.toString))
    }
  }
  
  def parseExprList(s: java.io.Reader): Try[List[SchemeVal]] = {
    parseAll(exprList, s) match {
      case Success(value, _) ⇒ Try(value)
      case failure ⇒ Try(throw new ParseException(failure.toString))
    }
  }
  
}

class ScameREPL {
   
  def eval(env: Env, token: SchemeVal): Try[SchemeVal] = token match {
    case n@SchemeInt(_) ⇒ Try(n)
    case b@SchemeBool(_) ⇒ Try(b)
    case s@SchemeString(_) ⇒ Try(s)
    case SchemeAtom(id) ⇒env.getVar(id)
    // quote form
    case SchemeList(List(SchemeAtom("quote"), value)) ⇒ Try(value)
    case SchemeList(List(SchemeAtom("if"), pred, conseq, alt)) ⇒
        eval(env, pred).flatMap { case SchemeBool(false) ⇒ eval(env, alt); case _ ⇒ eval(env, conseq) } 
    case SchemeList(List(SchemeAtom("set!"), SchemeAtom(variable), form)) ⇒ eval(env, form) flatMap (env.setVar(variable, _))
    
    // define a variable
    case SchemeList(List(SchemeAtom("define"), SchemeAtom(variable), form)) ⇒ eval(env, form) flatMap (env.defineVar(variable, _))
    
    //define a normal function
    case SchemeList(SchemeAtom("define") :: SchemeList(SchemeAtom(func) :: params) :: body) ⇒
      env.defineVar(func, SchemeFunc(params.map(_.toString), None, body, env))
      
    // define a variadic function
    case SchemeList(SchemeAtom("define") :: SchemeDottedList((SchemeAtom(func) :: params), varargs) :: body) ⇒
      env.defineVar(func, SchemeFunc(params.map(_.toString), Some(varargs.toString), body, env))
      
    // define an anonymous function, eg. (lambda (a b) (+ a b))
    case SchemeList(SchemeAtom("lambda") :: SchemeList(params) :: body) ⇒
      Try(SchemeFunc(params.map(_.toString), None, body, env))
      
    // define an anonymous variadic function, eg. (lambda (a .  b) (cons a b))
    case SchemeList(SchemeAtom("lambda") :: SchemeDottedList(params, varargs) :: body) ⇒
      Try(SchemeFunc(params.map(_.toString), Some(varargs.toString), body, env))
    case SchemeList(SchemeAtom("lambda") :: (varargs@SchemeAtom(_)) :: body) ⇒
      Try(SchemeFunc(Nil, Some(varargs.toString), body, env))
    
    case SchemeList(List(SchemeAtom("load"), SchemeString(filename))) ⇒ {
      val loadSucess = PrimitiveFuncs.sequence(
          (for (parseResults ← PrimitiveFuncs.loadSchemeFile(filename)) yield parseResults map (eval(env, _))).get
          ).isSuccess
      Try(SchemeBool(loadSucess))
    }
      
    // call a function 
    case SchemeList((function@SchemeAtom(_)) :: args) ⇒
      for (func ← eval(env, function); argVals ← PrimitiveFuncs.sequence(args.map(eval(env, _)))) yield applyFunc(func)(argVals).get
    case badForm ⇒ Try(throw new BadSpecialFormException("Unrecognized special form", badForm))
  }
  
  var prompt = "Scame>>> "
  
  var quitCmd = "quit"
  
  private def readPrompt() = {
    println(prompt)
    io.StdIn.readLine()
  }
  
  def run() {
    var stop = false
    val priEnv = PrimitiveFuncs.makeEnv
    while (!stop) {
      val line = readPrompt()
      if (line == quitCmd) stop = true
      else println(repl(priEnv, line))
    }
  }
  
  def runOnce(in: java.lang.CharSequence) {
    val priEnv = PrimitiveFuncs.makeEnv
    println(repl(priEnv, in))
  }
  
  def runOnce(in: java.io.Reader) {
    val priEnv = PrimitiveFuncs.makeEnv
    println(repl(priEnv, in))
  }
  
  def repl(env: Env, in: java.lang.CharSequence) = {
    import util._
    val form = ScameParser.parseExpr(in)
    form match {
      case Failure(e) ⇒ e.toString
      case Success(v) ⇒ eval(env, v) match {
        case Failure(ee) ⇒ ee.toString
        case Success(ss) ⇒ss.toString
      }
    }
  }
  
  def repl(env: Env, in: java.io.Reader) = {
    import util._
    val form = ScameParser.parseExpr(in)
    form match {
      case Failure(e) ⇒ e.toString
      case Success(v) ⇒ eval(env, v) match {
        case Failure(ee) ⇒ ee.toString
        case Success(ss) ⇒ss.toString
      }
    }
  }
  
  private def applyFunc(f: SchemeVal)(args: List[SchemeVal]): Try[SchemeVal] = f match {
    case SchemePrimitive(func) ⇒func(args)
    case SchemeFunc(params, varargs, body, closure) ⇒ 
      if (params.length != args.length && varargs == None || params.length > args.length)
        Try(throw NumArgsException(params.length, args))
      else {
        val remainArgs = args.drop(params.length)
        val tempEnv = new Env(closure)
        def bindVarArgs(varargs: Option[String], env: Env) = varargs match {
          case Some(argName) ⇒ env.bindVars(Map(argName → SchemeList(remainArgs)))
          case None ⇒ env
        }
        // evaluate every scheme token in the function body and take the last one as return value
        body.map(eval(bindVarArgs(varargs, tempEnv.bindVars(Map((params zip args): _*))), _)).last
      }
  }
}