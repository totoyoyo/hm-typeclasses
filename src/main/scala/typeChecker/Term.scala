package typeChecker

sealed trait Term

case class IntLiteral(value : Int) extends Term
case class BoolLiteral(value: Boolean) extends Term
case class VarTerm (var varName: String) extends Term
case class Succ(arg : Term) extends Term
case class IntEquals(arg1: Term, arg2: Term) extends Term
case class BoolEquals(arg1: Term, arg2: Term) extends Term
case class IfThenElse (con: Term, tBranch: Term, fBranch : Term)  extends Term
case class Lambda(arg: String, typ: Option[Type], body: Term) extends Term
case class App(func: Term, arg: Term) extends Term
case class Let(varname: String, right: Term, afterIn : Term) extends Term
case object unit extends Term

case class Over(name: String, typeA: Type , afterIn: Term) extends Term
case class Inst(name: String, typeA: Type, rhs: Term, afterIn: Term) extends Term
