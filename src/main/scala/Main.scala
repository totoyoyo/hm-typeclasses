import typeChecker.ConstraintsInference._
import typeChecker._


object Main {

  def main(args: Array[String]): Unit = {
    // Write your term here
    val term: Term = UnitTerm
    typeCheck(term, toPrint = true)
  }

}
