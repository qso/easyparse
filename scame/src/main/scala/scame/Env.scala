package scame

import scala.collection.mutable.HashMap
import scala.util._
import scala.collection._

class Env(val parentEnv: Env) {
  
  private val context = new HashMap[String, SchemeVal]
  def getVar(name: String): Try[SchemeVal] = Try {
    if (parentEnv == null) 
      context.getOrElse(name, throw new UnboundVarException("Getting an unbound variable", name))
    else
      context.getOrElse(name, parentEnv.getVar(name).get)
  }
  
  def setVar(name: String, value: SchemeVal): Try[SchemeVal] = Try {
    if (parentEnv == null) {
      if (context.get(name) == None) throw new UnboundVarException("Setting an unbound variable", name)
      else {
        context(name) = value
        value
      }
    } else {
      if (context.get(name) == None) parentEnv.setVar(name, value).get
      else {context(name) = value; value}
    }
  }
  
  def isBound(name: String): Boolean = {
    if (parentEnv == null) {
      if (context.get(name) == None) false else true
    } else {
      if (context.get(name) == None) parentEnv.isBound(name) else true
    }
  }
  
  def defineVar(name: String, value: SchemeVal): Try[SchemeVal] = Try{context(name) = value; value}
  
  def bindVars(vars: Map[String, SchemeVal]): this.type = {
    context ++= vars
    this
  }
}