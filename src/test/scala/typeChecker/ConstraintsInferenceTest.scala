package typeChecker

import org.scalatest.funsuite.AnyFunSuite

class ConstraintsInferenceTest extends AnyFunSuite {

  test("infer0") {
    // unit
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
    // SHOULD FAIL \x. y 0
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
    // let f = \x.x in f(true)
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
    // \f: X->X. \x: X. let g = f in g(x)

    //  (X->X) -> X -> X
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

  test("Eq on int") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. intequals(x,y) in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // eq 2 5


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


  test("Eq on bool") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. intequals(x,y) in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // eq true true


    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("a"), BoolType))),
        Inst("eq", FuncType(IntType, FuncType(IntType, BoolType)),
          Lambda("x", None, Lambda("y", None, IntEquals(VarTerm("x"),VarTerm("y")))
          ),
          Inst("eq", FuncType(BoolType, FuncType(BoolType, BoolType)),
            Lambda("x", None, Lambda("y", None, BoolEquals(VarTerm("x"),VarTerm("y")))
            ),
          App(App(VarTerm("eq"), BoolLiteral(true)),BoolLiteral(true))
        )
      ))
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

  test("Eq alone") {

    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. intequals(x,y) in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // eq

    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("a"), BoolType))),
        Inst("eq", FuncType(IntType, FuncType(IntType, BoolType)),
          Lambda("x", None, Lambda("y", None, IntEquals(VarTerm("x"),VarTerm("y")))
          ),
          Inst("eq", FuncType(BoolType, FuncType(BoolType, BoolType)),
            Lambda("x", None, Lambda("y", None, BoolEquals(VarTerm("x"),VarTerm("y")))
            ), VarTerm("eq")
        )
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



  test("Eq on int Bug") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. boolequals(x,y) in  HERE IS THE ERROR
    // eq 2 5


    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("a"), BoolType))),
        Inst("eq", FuncType(IntType, FuncType(IntType, BoolType)),
          Lambda("x", None, Lambda("y", None, BoolEquals(VarTerm("x"),VarTerm("y"))) // I supply wrong instance def
          ),
          App(App(VarTerm("eq"), IntLiteral(2)),IntLiteral(4))
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


}
