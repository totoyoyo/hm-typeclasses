package typeChecker

class Context(map: Map[String,Type], val overloadMap: Map[String,Type] = Map(), val instanceMap: Map[String,Map[Type,Term]] = Map()) extends Map[String, Type] {

  val innerMap: Map[String,Type] = map

  def getOverload(s: String) : Type = {
    overloadMap.get(s) match {
      case Some(t) => t match {
        case fa@ForallType(_, _) => instantiateForall(fa)
        case _ => t
      }
      case None => throw NoVarException(s"Cannot instantiate without an over $s.")
    }
  }

  def getSpecial(s: String) : Type = {
    this.get(s) match {
      case Some(t) => t match {
        case fa@ForallType(_, _) => instantiateForall(fa)
        case _ => t
      }
      case None => throw NoVarException(s"Variable $s does not exist in context.")
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

  def removed(key: String): Context = new Context(innerMap.removed(key),overloadMap, instanceMap)
  def updated[V1 >: Type](key: String, value: V1): Map[String,V1] = innerMap.updated(key,value)
  def get(key: String): Option[Type] = innerMap.get(key)
  def iterator: Iterator[(String, Type)] = innerMap.iterator

  def addNormal(key: String, value: Type) : Context = new Context(innerMap.updated(key,value), overloadMap, instanceMap)
  def addInstance(overName: String, typeofInstance: Type, instanceDef: Term): Context = {
    instanceMap.get(overName) match {
      case Some(otherMap) =>
        val newDeepMap = otherMap + (typeofInstance -> instanceDef)
        new Context(innerMap, overloadMap, instanceMap.updated(overName,newDeepMap))
      case None =>
        new Context(innerMap, overloadMap, instanceMap.updated(overName,Map(typeofInstance->instanceDef)))
    }
  }
  def addOverload(key: String, value: Type) : Context = new Context(innerMap,
    overloadMap.updated(key,value), instanceMap)



}

object Context {

  var forallVarCounter = 0

  def genTypeVarForAll(): TypeVar = {
    forallVarCounter += 1;
    return TypeVar("UNIV" + forallVarCounter.toString)
  }

}
