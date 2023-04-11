package typeChecker

import org.scalatest.funsuite.AnyFunSuite

class ConstraintsInferenceTest extends AnyFunSuite {

  test("infer1") {
    val exampleTerm = Lambda("x", Some(FuncType(TypeVar("X"), TypeVar("Y"))), App(VarTerm("x"), IntLiteral(0)))
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    PPrinter.pprint(c)
    val out = ConstraintsInference.unify(c._2)
    PPrinter.pprint(out)
  }

  test("infer2") {
    val exampleTerm = Lambda("x", None, App(VarTerm("x"), IntLiteral(0)))
    PPrinter.pprint(exampleTerm)
    val c = ConstraintsInference.infer(exampleTerm)
    PPrinter.pprint(c)
    val outSubs: Seq[TypeSubstitution] = ConstraintsInference.unify(c._2)
    val outType = TypeSubstitution.applySeqTypeSub(outSubs,c._1)
    print("OutputType:")
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
