package typeChecker

sealed trait Type

case object IntType extends Type
case object BoolType extends Type
case class FuncType(left: Type, right: Type) extends Type
case object UnitType extends Type
case class TypeVar(name: String) extends Type
case class ForallType(typeVar: TypeVar, body: Type) extends Type
