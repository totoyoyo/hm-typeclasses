package typeChecker

import org.scalatest.funsuite.AnyFunSuite

class ConstraintsInferenceTest extends AnyFunSuite {

  test("infer0") {
    val exampleTerm: Term = unit
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    println("Tuple of (output type, constraints)")
    PPrinter.pprint(c)
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c._2)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,c._1)
    print("OutputType:")
    PPrinter.pprint(outType)
    print("OutputMaps:")
    PPrinter.pprint(outSubs)
  }

  test("infer1") {
    // \x: X->Y . x 0
    val exampleTerm = Lambda("x", Some(FuncType(TypeVar("X"), TypeVar("Y"))), App(VarTerm("x"), IntLiteral(0)))
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    println("Tuple of (output type, constraints)")
    PPrinter.pprint(c)
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c._2)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,c._1)
    print("OutputType:")
    PPrinter.pprint(outType)
    print("OutputMaps:")
    PPrinter.pprint(outSubs)
  }

  test("infer2") {
    // \x. x 0

    // (INT -> Y) -> Y
    val exampleTerm = Lambda("x", None, App(VarTerm("x"), IntLiteral(0)))
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    println("Tuple of (output type, constraints)")
    PPrinter.pprint(c)
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c._2)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,c._1)
    print("OutputType:")
    PPrinter.pprint(outType)
    print("OutputMaps:")
    PPrinter.pprint(outSubs)
  }

  test("infer3") {
    val exampleTerm = Lambda("x", None, App(VarTerm("y"), IntLiteral(0)))
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    println("Tuple of (output type, constraints)")
    PPrinter.pprint(c)
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c._2)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,c._1)
    print("OutputType:")
    PPrinter.pprint(outType)
    print("OutputMaps:")
    PPrinter.pprint(outSubs)
  }

  test("infer4") {
    val exampleTerm = Let("f",Lambda("x", None, VarTerm("x")), App(VarTerm("f"),BoolLiteral(true)))
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    println("Tuple of (output type, constraints)")
    PPrinter.pprint(c)
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c._2)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,c._1)
    print("OutputType:")
    PPrinter.pprint(outType)
    print("OutputMaps:")
    PPrinter.pprint(outSubs)
  }

  test("infer5") {
    val exampleTerm = Lambda("f", Some(FuncType(TypeVar("X"), TypeVar("X"))),
      Lambda("x", Some(TypeVar("X")), Let("g",VarTerm("f"), App(VarTerm("g"),VarTerm("x")))))
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    println("Tuple of (output type, constraints)")

    PPrinter.pprint(c)
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c._2)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,c._1)
    print("Output Type:")
    PPrinter.pprint(outType)
    print("OutputMaps:")
    PPrinter.pprint(outSubs)
  }

  test("Over and Ins") {
    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("a"), BoolType))),
      Inst("eq", FuncType(IntType, FuncType(IntType, BoolType)),
        Lambda("x", None, Lambda("y", None, IntEquals(VarTerm("x"),VarTerm("y")))
      ),
        unit
      )
    )
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    println("Tuple of (output type, constraints)")

    PPrinter.pprint(c)
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c._2)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,c._1)
    print("Output Type:")
    PPrinter.pprint(outType)
    print("OutputMaps:")
    PPrinter.pprint(outSubs)
  }





  test("gen" ) {
    print(ConstraintsInference.genTypeVar())
    print(ConstraintsInference.genTypeVar())
    print(ConstraintsInference.genTypeVar())
    print(ConstraintsInference.genTypeVar())
  }


}
