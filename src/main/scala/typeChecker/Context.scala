package typeChecker

import scala.collection.mutable

class Context extends mutable.HashMap[String,Type] {

  var forallVarCounter = 0

  def genTypeVar(): String = {
    forallVarCounter += 1;
    return "UNIV." + forallVarCounter.toString
  }


  def getSpecial(s: String) : Type = {
    this.get(s) match {
      case Some(t) => t match {
        case ForallType(typeVar, body) => ???
        case _ => t
      }
      case None => ???
    }
  }

  def instantiateForall(fa: ForallType) : Type = {
    fa.body
  }



}
