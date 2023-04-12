package typeChecker

import scala.collection.immutable.{AbstractSet, SortedSet}

object ConstraintsInference {

  var typeVarCounter = 0

  def genTypeVar(): String = {
    typeVarCounter += 1;
    return "X" + typeVarCounter.toString
  }

  private def infer(term: Term, context: Context): (Type, Set[Constraint]) = {
    term match {
      case IntLiteral(value) => return (IntType, Set())
      case BoolLiteral(value) => return (BoolType, Set())
      case vt@VarTerm(varName) =>
//        vt.varName = "haha"
        (context.getSpecial(varName),Set())
      case Succ(arg) =>
        val (type1,constraints)  = infer(arg,context)
        return (IntType, constraints + Constraint(type1, IntType))
      case IntEquals(a1, a2) =>
        val (type1,constraints1)  = infer(a1,context)
        val (type2,constraints2)  = infer(a2,context)
        return (BoolType, constraints1 ++ constraints2 + Constraint(type1, IntType) + Constraint(type2, IntType))
      case BoolEquals(a1, a2) =>
        val (type1,constraints1)  = infer(a1,context)
        val (type2,constraints2)  = infer(a2,context)
        return (BoolType, constraints1 ++ constraints2 + Constraint(type1, BoolType) + Constraint(type2, BoolType))
      case Lambda(arg, typ, body) =>
        val t1: Type = typ.getOrElse(TypeVar(genTypeVar()))
        val newContext: Context = context.addNormal(arg,t1)
        val (t2,constraints) = infer(body, newContext)
        return (FuncType(t1,t2), constraints)
      case App(func, arg) =>
        val (t1,c1) = infer(func,context)
        val (t2,c2) = infer(arg,context)
        val newX = TypeVar(genTypeVar())
        val newConstraints = c1.union(c2) + Constraint(t1, FuncType(t2,newX))
        return (newX, newConstraints)
      case Let(varname, right, afterIn) =>
        val (s1,c1) = infer(right,context)
        val principal = TypeSubstitution.applySeqTypeSub(unify(c1), s1)
        val generalizedT = Type.generalizeLet(principal, context)
        val newContext = context.addNormal(varname,generalizedT)
        return infer(afterIn,newContext)
      case IfThenElse(con, tBranch, fBranch) =>
        val (t1,c1) = infer(con,context)
        val (t2,c2) = infer(tBranch,context)
        val (t3,c3) = infer(fBranch,context)
        val cPrime = c1.union(c2).union(c3) + Constraint(t1, BoolType) + Constraint(t2, t3)
        return (t2, cPrime)
      case Over(name, typeA, afterIn) =>
        val newContext = context.addOverload(name, typeA) // this is inserted into the same maps as others
        infer(afterIn,newContext)
      case Inst(name,typeA, rhs, afterIn) =>
        // Look up the overload declaration type, add a constraint that it can be this
        val lookedupType = context.getOverload(name)
        val c0 = Constraint(lookedupType, typeA)

        // infer type of rhs
        val (t1,c1) = infer(rhs,context)
        val uniout = unify(c1 + c0);

        val newContext = context.addInstance(name,typeA,rhs)
        infer(afterIn,newContext)
      case unit => return (UnitType, Set())
    }
  }

  def infer(termOuter: Term) : (Type, Set[Constraint]) = {
    infer(termOuter, new Context(Map.empty))
  }

  def unify(c: Set[Constraint]) : Seq[TypeSubstitution] = {
    if (c.isEmpty) {
      Seq.empty
    } else {
      val head = c.head
      val cPrime = c - head
      val s = head.left
      val t = head.right
      (s,t) match {
        case _ if s == t => unify(cPrime)
        case (tv@TypeVar(_), _) if !occurCheck(tv,t) =>
          val newSub = new TypeSubstitution(tv, t)
          Seq(newSub) ++ unify(newSub.substituteOnConstraints(cPrime))
        case (_, tv@TypeVar(_)) if !occurCheck(tv,s) =>
          val newSub = new TypeSubstitution(tv, s)
          Seq(newSub) ++ unify(newSub.substituteOnConstraints(cPrime))
        case (FuncType(s1, s2), FuncType(t1, t2)) =>
          val newConstraints: Set[Constraint] = cPrime + Constraint(s1,t1) + Constraint(s2,t2)
          unify(newConstraints)
        case _ =>
          throw CannotUnify(s"Cannot unify ${s.toString} and ${t.toString}")
      }
    }
  }


  def occurCheck(t1: TypeVar, t2: Type): Boolean = {
      t2 match {
        case IntType => false
        case BoolType => false
        case FuncType(left, right) => occurCheck(t1,left) || occurCheck(t1,right)
        case UnitType => false
        case TypeVar(name2) => t1.name == name2
        case ForallType(typeVar, body) => ???
      }
  }




  def composeMaps(leftMap: Map[Type,Type], rightMap: Map[Type,Type]): Map[Type,Type] = {
    Map.empty
  }





}
