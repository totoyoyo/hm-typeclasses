package typeChecker

class Context(map: Map[String,Type]) extends Map[String, Type] {

  val innerMap: Map[String,Type] = map

  def getSpecial(s: String) : Type = {
    this.get(s) match {
      case Some(t) => t match {
        case fa@ForallType(_, _) => instantiateForall(fa)
        case _ => t
      }
      case None => ???
    }
  }


  def instantiateForall(fa: ForallType) : Type = {
    val tv = Context.genTypeVarForAll()
    val ts = new TypeSubstitution(fa.typeVar: TypeVar, tv: Type)
    val newBody = ts.substitute(fa.body)
    newBody match {
      case next@ForallType(_, _) => instantiateForall(next)
      case _ => newBody
    }
  }

  def removed(key: String): Context = new Context(innerMap.removed(key))
  def updated[V1 >: Type](key: String, value: V1): Map[String,V1] = innerMap.updated(key,value)
  def get(key: String): Option[Type] = innerMap.get(key)
  def iterator: Iterator[(String, Type)] = innerMap.iterator

  def addSpecial(key: String, value: Type) : Context = new Context(innerMap.updated(key,value))


}

object Context {

  var forallVarCounter = 0

  def genTypeVarForAll(): TypeVar = {
    forallVarCounter += 1;
    return TypeVar("UNIV" + forallVarCounter.toString)
  }

}
