import typeChecker.ConstraintsInference._
import typeChecker._


object Main {

  def main(args: Array[String]): Unit = {
    // Write your term here
    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("a"), BoolType))),
        Inst("eq", FuncType(IntType, FuncType(IntType, BoolType)),
          Lambda("x", None, Lambda("y", None, IntEquals(VarTerm("x"),VarTerm("y")))
          ),
          Inst("eq", FuncType(BoolType, FuncType(BoolType, BoolType)),
            Lambda("x", None, Lambda("y", None, BoolEquals(VarTerm("x"),VarTerm("y")))
            ),
            App(App(VarTerm("eq"), IntLiteral(2)),IntLiteral(5))
          )
        ))
    typeCheck(exampleTerm, toPrint = true)
  }

}
