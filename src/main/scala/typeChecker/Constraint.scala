package typeChecker


sealed trait Constraint

case class EqualityConstraint(left : Type, right: Type) extends Constraint
case class InstanceConstraint(name: String, t: Type, c: Context, checked: Boolean) extends Constraint
