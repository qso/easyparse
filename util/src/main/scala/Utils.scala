package easyparse

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


/**
  * Created by qso on 16-1-25.
  */
object Macros {
  case class FuncName(name: String)
  def funcNameImpl(c: Context): c.Expr[FuncName] = {
    import c.universe._

    val sym = c.internal.enclosingOwner
    val name = sym.name.decodedName.toString.trim
    c.Expr[FuncName](q"easyparse.Macros.FuncName($name)")
  }
}


