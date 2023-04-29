package typeChecker

import org.scalatest.funsuite.AnyFunSuite

class ConstraintsInferenceTest extends AnyFunSuite {

  test("infer0") {
    // ()
    // Unit
    val exampleTerm: Term = UnitTerm
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("infer1") {
    // \x: X->Y . x 0
    // (X->Y) -> Y
    val exampleTerm = Lambda("x", Some(FuncType(TypeVar("X"), TypeVar("Y"))), App(VarTerm("x"), IntLiteral(0)))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("infer2") {
    // \x. x 0
    // (INT -> Y) -> Y
    val exampleTerm = Lambda("x", None, App(VarTerm("x"), IntLiteral(0)))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("infer3") {
    // SHOULD FAIL \x. y 0
    val exampleTerm = Lambda("x", None, App(VarTerm("y"), IntLiteral(0)))
    try {
      ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
      fail("Should not get here")
    } catch {
      case _ : NoVarException =>
    }

  }

  test("infer4") {
    // let f = \x.x in f(true)
    // Bool
    val exampleTerm = Let("f",Lambda("x", None, VarTerm("x")), App(VarTerm("f"),BoolLiteral(true)))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("infer5") {
    // \f: X->X. \x: X. let g = f in g(x)
    //  (X->X) -> X -> X
    val exampleTerm = Lambda("f", Some(FuncType(TypeVar("X"), TypeVar("X"))),
      Lambda("x", Some(TypeVar("X")), Let("g",VarTerm("f"), App(VarTerm("g"),VarTerm("x")))))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("Eq on int") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. intequals(x,y) in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // eq 2 5
    // Bool
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
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)

  }


  test("Eq on bool") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. intequals(x,y) in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // eq true true
    // Bool
    val exampleTerm = {
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
    }
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("Eq alone") {
    // Should fail, ambiguous instance
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
    try {
      ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
      fail("Should not pass")
    } catch {
      case _ : AmbiguousTypeClass =>
    }

  }

  test("Eq bool one arg") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. intequals(x,y) in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // eq false

    // Bool -> Bool

    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("a"), BoolType))),
        Inst("eq", FuncType(IntType, FuncType(IntType, BoolType)),
          Lambda("x", None, Lambda("y", None, IntEquals(VarTerm("x"),VarTerm("y")))
          ),
          Inst("eq", FuncType(BoolType, FuncType(BoolType, BoolType)),
            Lambda("x", None, Lambda("y", None, BoolEquals(VarTerm("x"),VarTerm("y")))
            ),
            App(VarTerm("eq"),BoolLiteral(false))
          )
        )
      )
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("Eq only 1 instance") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // eq

    // Bool -> Bool -> Bool

    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("a"), BoolType))),
          Inst("eq", FuncType(BoolType, FuncType(BoolType, BoolType)),
            Lambda("x", None, Lambda("y", None, BoolEquals(VarTerm("x"),VarTerm("y")))
            ),
            (VarTerm("eq"))
          )
      )
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("iden int") {

    // over iden :: \/a. a -> a in
    // inst iden :: Int-> int = \x.  x in
    // inst iden :: Bool -> bool = \x. x in
    // iden 2
    // Int

    val exampleTerm =
      Over("iden",ForallType(TypeVar("a"),FuncType(TypeVar("a"), TypeVar("a"))),
        Inst("iden", FuncType(BoolType, BoolType),
          Lambda("x", None,  VarTerm("x")),
          Inst("iden", FuncType(IntType, IntType),
            Lambda("x", None,  VarTerm("x")),
          App(VarTerm("iden"), IntLiteral(2))
        )
      ))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }



  test("Eq on int, incorrect instance def") {
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

    try {
      ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
      fail("Should not pass")
    } catch {
      case _: CannotUnify =>
    }
  }



  test("instance used inside a let RHS") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. intequals(x,y) in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // let eqq = eq 2
    // eqq
    // Int -> Bool
    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("a"), BoolType))),
        Inst("eq", FuncType(BoolType, FuncType(BoolType, BoolType)),
          Lambda("x", None, Lambda("y", None, BoolEquals(VarTerm("x"),VarTerm("y"))) // I supply wrong instance def
          ), Inst("eq", FuncType(IntType, FuncType(IntType, BoolType)),
            Lambda("x", None, Lambda("y", None, IntEquals(VarTerm("x"),VarTerm("y")))
            ),
          Let("eqq" , App(VarTerm("eq"), IntLiteral(2)) ,
            VarTerm("eqq")
          )
        )
      ))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }


  test("Different instances of eq in different lets") {
    // over eq :: \/a. a -> a -> bool in
    // inst eq :: Int-> int -> bool = \x.\y. intequals(x,y) in
    // inst eq :: Bool -> Bool -> bool = \x.\y. boolequals(x,y) in
    // let eq1Int = eq 2
    // let eq1Bool = eq True
    // eq1Bool
    // Bool -> Bool
    val exampleTerm =
      Over("eq",ForallType(TypeVar("a"),FuncType(TypeVar("a"), FuncType(TypeVar("b"), BoolType))),
        Inst("eq", FuncType(BoolType, FuncType(BoolType, BoolType)),
          Lambda("x", None, Lambda("y", None, BoolEquals(VarTerm("x"),VarTerm("y"))) // I supply wrong instance def
          ), Inst("eq", FuncType(IntType, FuncType(IntType, BoolType)),
            Lambda("x", None, Lambda("y", None, IntEquals(VarTerm("x"),VarTerm("y")))
            ),
            Let("eq1Int" , App(VarTerm("eq"), IntLiteral(2)) ,
              Let("eq1Bool", App(VarTerm("eq"), BoolLiteral(true)),
                VarTerm("eq1Bool")
              )
            )
          )
        ))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("coerce example") {
//    over coerce:\/A. \/B. (A -> B) in
//    inst coerce:(Bool -> Int) = (lambda x.if (x)
//            then (1)
//            else (0)) in
//    inst coerce:(Int -> Bool) = (lambda x.x (==_int) 1) in
//    (coerce false)
    //  Int
    val exampleTerm =
      Over("coerce",ForallType(TypeVar("A"), ForallType(TypeVar("B"), FuncType(TypeVar("A"), TypeVar("B")))),
        Inst("coerce", FuncType(BoolType, IntType),
          Lambda("x", None, IfThenElse(VarTerm("x"), IntLiteral(1), IntLiteral(0))),
          Inst("coerce", FuncType(IntType, BoolType),
            Lambda("x", None, IntEquals(VarTerm("x"), IntLiteral(1))),
            App(VarTerm("coerce"),BoolLiteral(false))
          )
        ))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("coerce use inside a lambda") {
    // over coerce:\/A. \/B. (A -> B) in
    //inst coerce:(Bool -> Int) = (lambda x.if (x)
    //        then (1)
    //        else (0)) in
    //inst coerce:(Int -> Bool) = (lambda x.x (==_int) 1) in
    //((lambda x.(coerce x)) 1)
    // Bool

    val exampleTerm =
      Over("coerce",ForallType(TypeVar("A"), ForallType(TypeVar("B"), FuncType(TypeVar("A"), TypeVar("B")))),
        Inst("coerce", FuncType(BoolType, IntType),
          Lambda("x", None, IfThenElse(VarTerm("x"), IntLiteral(1), IntLiteral(0))),
          Inst("coerce", FuncType(IntType, BoolType),
            Lambda("x", None, IntEquals(VarTerm("x"), IntLiteral(1))),
            App(Lambda("x", None, App(VarTerm("coerce"),VarTerm("x"))), IntLiteral(1))
          )
        ))
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("coerce instance nested inside a lambda") {
    // over coerce:\/A. \/B. (A -> B) in
    // inst coerce:(Bool -> Int) = (lambda x.if (x)
    //    then (1)
    //   else (0)) in
    // ((lambda x. inst coerce:(Int -> Bool) = (lambda x.x (==_int) 1) in
    // (coerce x)) 1)
    // Bool

    val exampleTerm =
      Over("coerce",ForallType(TypeVar("A"), ForallType(TypeVar("B"), FuncType(TypeVar("A"), TypeVar("B")))),
        Inst("coerce", FuncType(BoolType, IntType),
          Lambda("x", None, IfThenElse(VarTerm("x"), IntLiteral(1), IntLiteral(0))),
            App(Lambda("x", None,
              Inst("coerce", FuncType(IntType, BoolType),
                Lambda("x", None, IntEquals(VarTerm("x"), IntLiteral(1))),
              App(VarTerm("coerce"),VarTerm("x")))), IntLiteral(1))
          )
        )
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }

  test("coerce def in let") {
    // Let coer = over coerce:\/A. \/B. (A -> B) in
    //    inst coerce:(Int -> Bool) = (lambda x.x (==_int) 1) in
    //     coerce in
    // coer
    // Int -> Bool

    val exampleTerm = {
      Let("coer",Over("coerce",ForallType(TypeVar("A"), ForallType(TypeVar("B"),
        FuncType(TypeVar("A"), TypeVar("B")))), Inst("coerce", FuncType(IntType, BoolType),
            Lambda("x", None, IntEquals(VarTerm("x"), IntLiteral(1))), VarTerm("coerce"))), VarTerm("coer"))
    }
    ConstraintsInference.typeCheck(exampleTerm, toPrint = true)
  }


}
