package typeChecker

sealed trait Term

case class IntLiteral(value : Int) extends Term {
  override def toString: String = value.toString
}
case class BoolLiteral(value: Boolean) extends Term {
  override def toString: String = value.toString
}
case class VarTerm (varName: String) extends Term {
  override def toString: String = varName
}
case class Succ(arg : Term) extends Term {
  override def toString: String = s"Succ(${arg.toString})"
}
case class IntEquals(arg1: Term, arg2: Term) extends Term {
  override def toString: String = s"${arg1.toString} (==_int) ${arg2.toString}"
}
case class BoolEquals(arg1: Term, arg2: Term) extends Term {
  override def toString: String = s"${arg1.toString} (==_bool) ${arg2.toString}"
}
case class IfThenElse (con: Term, tBranch: Term, fBranch : Term)  extends Term {
  override def toString: String = s"if (${con.toString}) \nthen (${tBranch.toString}) \nelse (${fBranch.toString})"
}
case class Lambda(arg: String, typ: Option[Type], body: Term) extends Term{
  override def toString: String = {
    val argString = s"lambda ${arg}"
    val typeAnnotation = typ match {
      case Some(value) => s":${value.toString}."
      case None => "."
    }
    return argString + typeAnnotation + body.toString
  }
}
case class App(func: Term, arg: Term) extends Term {
  override def toString: String = s"(${func.toString} ${arg.toString})"
}
case class Let(varname: String, right: Term, afterIn : Term) extends Term {
  override def toString: String = s"Let $varname = ${right.toString} in \n${afterIn.toString}"
}
case object unit extends Term {
  override def toString: String = s"()"
}

case class Over(name: String, typeA: Type , afterIn: Term) extends Term {
  override def toString: String = s"over $name:${typeA.toString} in \n${afterIn.toString}"
}
case class Inst(name: String, typeA: Type, rhs: Term, afterIn: Term) extends Term {
  override def toString: String = s"inst $name:${typeA.toString} = ${rhs.toString} in \n${afterIn.toString}"
}
