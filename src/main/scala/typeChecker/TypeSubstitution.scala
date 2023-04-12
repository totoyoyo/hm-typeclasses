package typeChecker

class TypeSubstitution(left: TypeVar, right: Type) {

  var map: Map[TypeVar, Type] = Map.empty + (left -> right)

  override def toString: String = map.toString()

  def add(left: TypeVar, right: Type): Unit = {
    map = map + (left -> right)
  }

  def substitute(t: Type): Type = {
    t match {
      case IntType => IntType
      case BoolType => BoolType
      case FuncType(left, right) => FuncType(substitute(left), substitute(right))
      case UnitType => UnitType
      case tv@TypeVar(name) => map.getOrElse(tv, t)
      case ForallType(typeVar, body) => ???
    }
  }


  def substituteOnConstraints(cs: Set[Constraint]): Set[Constraint] = {
    cs.map {
      case EqualityConstraint(left, right) => EqualityConstraint(substitute(left), substitute(right))
      case InstanceConstraint(name, t) => InstanceConstraint(name, substitute(t))
    }
  }


}

object TypeSubstitution {

  def applySeqTypeSub(ts: Seq[TypeSubstitution], t: Type): Type = {
    var tOut = t
    ts.foreach(tSub => tOut = tSub.substitute(tOut))
    return tOut
  }

}
