package typeChecker

import typeChecker.PPrinter.pprint

import scala.collection.immutable.{AbstractSet, SortedSet}

object ConstraintsInference {

  var typeVarCounter = 0

  def genTypeVar(): String = {
    typeVarCounter += 1;
    return "X" + typeVarCounter.toString
  }


  // Returns the context  of the highest level after Over and Inst
  private def infer(term: Term, context: Context): (Type, Seq[Constraint]) = {
    term match {
      case IntLiteral(value) => return (IntType, Seq())
      case BoolLiteral(value) => return (BoolType, Seq())
      case vt@VarTerm(varName) =>
        context.overloadMap.get(varName) match {
          case None => (context.getSpecial(varName),Seq())
          case _ =>
            val typeofInst = context.getOverload(varName)
            (typeofInst, Seq(InstanceConstraint(varName, typeofInst, context, checked = false)))
        }
      case Succ(arg) =>
        val (type1,constraints)  = infer(arg,context)
        return (IntType, constraints :+ EqualityConstraint(type1, IntType))
      case IntEquals(a1, a2) =>
        val (type1,constraints1)  = infer(a1,context)
        val (type2,constraints2)  = infer(a2,context)
        return (BoolType, constraints1 ++ constraints2 :+ EqualityConstraint(type1, IntType) :+
          EqualityConstraint(type2, IntType))
      case BoolEquals(a1, a2) =>
        val (type1,constraints1)  = infer(a1,context)
        val (type2,constraints2)  = infer(a2,context)
        return (BoolType, constraints1 ++ constraints2 :+ EqualityConstraint(type1, BoolType) :+
          EqualityConstraint(type2, BoolType))
      case Lambda(arg, typ, body) =>
        val t1: Type = typ.getOrElse(TypeVar(genTypeVar()))
        val newContext: Context = context.addNormal(arg,t1)
        val (t2,constraints) = infer(body, newContext)
        return (FuncType(t1,t2), constraints)
      case App(func, arg) =>
        val (t1,c1) = infer(func,context)
        val (t2,c2) = infer(arg,context)
        val newX = TypeVar(genTypeVar())
        val newConstraints = c1 ++ c2 :+ EqualityConstraint(t1, FuncType(t2,newX))
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
        val cPrime = c1 ++ c2 ++ c3 :+ EqualityConstraint(t1, BoolType) :+ EqualityConstraint(t2, t3)
        return (t2, cPrime)
      case Over(name, typeA, afterIn) =>
        val newContext = context.addOverload(name, typeA) // this is inserted into the same maps as others
        infer(afterIn,newContext)
      case Inst(name,typeA, rhs, afterIn) =>
        // Look up the overload declaration type, add a constraint that it can be this
        val lookedupType = context.getOverload(name)
        val c0 = EqualityConstraint(lookedupType, typeA)
        unify(Seq(c0))

        // infer type of rhs
        val (t1,c1) = infer(rhs,context)
        val principal = TypeSubstitution.applySeqTypeSub(unify(c1), t1)
        unify(Seq(EqualityConstraint(typeA, principal)))

        val newContext = context.addInstance(name,typeA,rhs)
        infer(afterIn,newContext)
      case unit => return (UnitType, Seq())
    }
  }

  def infer(termOuter: Term) : (Type, Seq[Constraint]) = {
    infer(termOuter, new Context(Map.empty))
  }


  def typeCheck(termOuter: Term) : Type = {
    val (t,c) =  infer(termOuter, new Context(Map.empty))
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,t)
    outType
  }


  def unify(c: Seq[Constraint]) : Seq[TypeSubstitution] = {
    if (c.isEmpty) {
      Seq.empty
    } else {
      val head = c.head
      val cPrime = c.tail
      head match {
        case EqualityConstraint(left, right) =>
          val s = left
          val t = right
          (s,t) match {
            case _ if s == t => unify(cPrime)
            case (tv@TypeVar(_), _) if !occurCheck(tv,t) =>
              val newSub = new TypeSubstitution(tv, t)
              Seq(newSub) ++ unify(newSub.substituteOnConstraints(cPrime))
            case (_, tv@TypeVar(_)) if !occurCheck(tv,s) =>
              val newSub = new TypeSubstitution(tv, s)
              Seq(newSub) ++ unify(newSub.substituteOnConstraints(cPrime))
            case (FuncType(s1, s2), FuncType(t1, t2)) =>
              val newConstraints: Seq[Constraint] = cPrime :+ EqualityConstraint(s1,t1) :+ EqualityConstraint(s2,t2)
              unify(newConstraints)
            case _ =>
              throw CannotUnify(s"Cannot unify ${s.toString} and ${t.toString}")
          }
        case InstanceConstraint(name, t, c0, bool) =>
          // Lookup from instance map
          c0.instanceMap.get(name) match {
            // If there is instance, check if instance has a matching type
            case Some(typeToTerm) => typeToTerm.get(t) match {
              // If it does, remove the constraint and continue
              case Some(term) => unify(cPrime)
              // If not, first check if all remaining constraints are instance constraints
              case None =>
                // If there is only one instance, add constraint that t must be that instance's type
                if (typeToTerm.size == 1) {
                  val (typeInstance, _) = typeToTerm.head
                  val newConstraints: Seq[Constraint] = cPrime :+ EqualityConstraint(t, typeInstance) :+ head
                  return unify(newConstraints)
                }
                val allInstanceConstraints = cPrime.forall {
                  p => p match {
                    case InstanceConstraint(_, _, _, _)  => true
                    case _ => false
                  }
                }
                // If not all constraints are instance constraints, more can be inferred, so continue
                if (!allInstanceConstraints) {
                  unify(cPrime :+ head)
                } else {
                  // If all remaining constaints are instance contraints, go through all at least once
                  val newHead = InstanceConstraint(name, t, c0, checked = true)
                  val allInstanceChecked = cPrime.forall {
                    p => p match {
                      case InstanceConstraint(_, _, _, checked) if checked  => true
                      case _ => false
                    }
                  }
                  // All instance checked, its over, else keep unifying
                  if (allInstanceChecked) {
                    print("Cannot resolve the following ambiguous instance constraints:")
                    pprint(c)
                    throw AmbiguousTypeClass(s"Typeclass constraints are ambiguous.")
                  } else (unify(cPrime :+ newHead))
                }
            }
            case None => throw CannotUnify(s"Cannot unify. Instance of overloaded function `${name}` does not exist")
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
