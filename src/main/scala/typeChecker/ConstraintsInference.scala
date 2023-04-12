package typeChecker

import scala.collection.immutable.{AbstractSet, SortedSet}

object ConstraintsInference {

  var typeVarCounter = 0

  def genTypeVar(): String = {
    typeVarCounter += 1;
    return "X" + typeVarCounter.toString
  }


  // Returns the context  of the highest level after Over and Inst
  private def infer(term: Term, context: Context): (Type, Set[Constraint], Context) = {
    term match {
      case IntLiteral(value) => return (IntType, Set(), context)
      case BoolLiteral(value) => return (BoolType, Set(), context)
      case vt@VarTerm(varName) =>
        context.overloadMap.get(varName) match {
          case None => (context.getSpecial(varName),Set(), context)
          case _ =>
            val typeofInst = context.getOverload(varName)
            (typeofInst, Set(InstanceConstraint(varName, typeofInst)), context)
        }
      case Succ(arg) =>
        val (type1,constraints, _)  = infer(arg,context)
        return (IntType, constraints + EqualityConstraint(type1, IntType), context)
      case IntEquals(a1, a2) =>
        val (type1,constraints1, _)  = infer(a1,context)
        val (type2,constraints2, _)  = infer(a2,context)
        return (BoolType, constraints1 ++ constraints2 + EqualityConstraint(type1, IntType) +
          EqualityConstraint(type2, IntType), context)
      case BoolEquals(a1, a2) =>
        val (type1,constraints1,_)  = infer(a1,context)
        val (type2,constraints2,_ )  = infer(a2,context)
        return (BoolType, constraints1 ++ constraints2 + EqualityConstraint(type1, BoolType) +
          EqualityConstraint(type2, BoolType), context)
      case Lambda(arg, typ, body) =>
        val t1: Type = typ.getOrElse(TypeVar(genTypeVar()))
        val newContext: Context = context.addNormal(arg,t1)
        val (t2,constraints,_) = infer(body, newContext)
        return (FuncType(t1,t2), constraints, context)
      case App(func, arg) =>
        val (t1,c1, _) = infer(func,context)
        val (t2,c2, _) = infer(arg,context)
        val newX = TypeVar(genTypeVar())
        val newConstraints = c1.union(c2) + EqualityConstraint(t1, FuncType(t2,newX))
        return (newX, newConstraints, context)
      case Let(varname, right, afterIn) =>
        val (s1,c1, _) = infer(right,context)
        val principal = TypeSubstitution.applySeqTypeSub(unify(c1, context), s1)
        val generalizedT = Type.generalizeLet(principal, context)
        val newContext = context.addNormal(varname,generalizedT)
        return infer(afterIn,newContext)
      case IfThenElse(con, tBranch, fBranch) =>
        val (t1,c1,_) = infer(con,context)
        val (t2,c2,_) = infer(tBranch,context)
        val (t3,c3,_) = infer(fBranch,context)
        val cPrime = c1.union(c2).union(c3) + EqualityConstraint(t1, BoolType) + EqualityConstraint(t2, t3)
        return (t2, cPrime, context)
      case Over(name, typeA, afterIn) =>
        val newContext = context.addOverload(name, typeA) // this is inserted into the same maps as others
        infer(afterIn,newContext)
      case Inst(name,typeA, rhs, afterIn) =>
        // Look up the overload declaration type, add a constraint that it can be this
        val lookedupType = context.getOverload(name)
        val c0 = EqualityConstraint(lookedupType, typeA)
        unify(Set(c0), context)

        // infer type of rhs
        val (t1,c1,_) = infer(rhs,context)
        val principal = TypeSubstitution.applySeqTypeSub(unify(c1, context), t1)
        unify(Set(EqualityConstraint(typeA, principal)), context)

        val newContext = context.addInstance(name,typeA,rhs)
        infer(afterIn,newContext)
      case unit => return (UnitType, Set(), context)
    }
  }

  def infer(termOuter: Term) : (Type, Set[Constraint], Context) = {
    infer(termOuter, new Context(Map.empty))
  }

  def unify(c: Set[Constraint], context: Context) : Seq[TypeSubstitution] = {
    if (c.isEmpty) {
      Seq.empty
    } else {
      val head = c.head
      val cPrime = c - head
      head match {
        case EqualityConstraint(left, right) =>
          val s = left
          val t = right
          (s,t) match {
            case _ if s == t => unify(cPrime, context)
            case (tv@TypeVar(_), _) if !occurCheck(tv,t) =>
              val newSub = new TypeSubstitution(tv, t)
              Seq(newSub) ++ unify(newSub.substituteOnConstraints(cPrime), context)
            case (_, tv@TypeVar(_)) if !occurCheck(tv,s) =>
              val newSub = new TypeSubstitution(tv, s)
              Seq(newSub) ++ unify(newSub.substituteOnConstraints(cPrime), context)
            case (FuncType(s1, s2), FuncType(t1, t2)) =>
              val newConstraints: Set[Constraint] = cPrime + EqualityConstraint(s1,t1) + EqualityConstraint(s2,t2)
              unify(newConstraints, context)
            case _ =>
              throw CannotUnify(s"Cannot unify ${s.toString} and ${t.toString}")
          }
        case InstanceConstraint(name, t) =>
          context.instanceMap.get(name) match {
            case Some(typeToTerm) => typeToTerm.get(t) match {
              case Some(term) => unify(cPrime, context)
              case None =>
                val allInstanceConstraints = cPrime.forall {
                  p => p match {
                    case EqualityConstraint(left, right) => false
                    case InstanceConstraint(name, t) => true
                  }
                }
                if (allInstanceConstraints) Seq() else (unify(cPrime + head, context))
            }
            case None => throw CannotUnify(s"Cannot unify. Overloaded function `${name}` does not exist")
          }

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
