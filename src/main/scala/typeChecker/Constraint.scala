package typeChecker


sealed trait Constraint

case class EqualityConstraint(left : Type, right: Type) extends Constraint
case class InstanceConstraint(name: String, t: Type) extends Constraint
