package typeChecker


sealed trait Constraint

case class EqualityConstraint(left : Type, right: Type) extends Constraint {
  override def toString: String = s"EqualityConstraint: ${left.toString} == ${right.toString}"
}
case class InstanceConstraint(name: String, t: Type, c: Context, checked: Boolean) extends Constraint {
  override def toString: String = s"InstanceConstraint: $name has type ${t.toString}"
}
