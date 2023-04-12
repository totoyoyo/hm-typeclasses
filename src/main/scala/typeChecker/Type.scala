package typeChecker

sealed trait Type

case object IntType extends Type
case object BoolType extends Type
case class FuncType(left: Type, right: Type) extends Type
case object UnitType extends Type
case class TypeVar(name: String) extends Type
case class ForallType(typeVar: TypeVar, body: Type) extends Type
case class DictType(ts:Seq[Type]) extends Type


object Type {

  def generalizeLet(t: Type, context: Map[String, Type]): Type = {
    val typeVars = collectTypeVars(t)
    var contextTypeVars: Set[TypeVar] = Set()
    context.foreach{
      case (_,t) => contextTypeVars = contextTypeVars ++ collectTypeVars(t)
    }
    val toGeneralize = typeVars -- contextTypeVars
    toGeneralize.foldLeft(t)((t1,tv) => ForallType(tv,t1))
  }


  private def collectTypeVars(t: Type): Set[TypeVar] = {
    t match {
      case IntType => Set()
      case BoolType => Set()
      case FuncType(left, right) => collectTypeVars(left) ++ collectTypeVars(right)
      case UnitType => Set()
      case tv @ TypeVar(name) => Set(tv)
      case ForallType(typeVar, body) => collectTypeVars(body) - typeVar
      case DictType(ts) => ???
    }
  }

}
