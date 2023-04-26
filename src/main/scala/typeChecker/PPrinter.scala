package typeChecker
// Pretty printer adapted from https://stackoverflow.com/questions/15718506/scala-how-to-print-case-classes-like-pretty-printed-tree

object PPrinter {
  def pprint(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit = {

    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match {
      case _: Iterable[Any] => ""
      case typ: Type => typ.toString
      case term: Term => term.toString
      case constraint: Constraint => constraint.toString
      case obj: Product => obj.productPrefix
      case _ => obj.toString }

    println(s"$indent$prettyName$ptype")

    obj match {
      case seq: Iterable[Any] =>
        seq.foreach(pprint(_, depth + 1))
      case _: Type | _: Term | _: Constraint =>
      case obj: Product =>
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) => pprint(subObj, depth + 1, Some(paramName)) }
      case _ =>
    }
  }
  def pprintToString(obj: Any, depth: Int = 0, paramName: Option[String] = None): String = {
    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match {
      case _: Iterable[Any] => ""
      case typ: Type => return typ.toString
      case term: Term => return term.toString
      case constraint: Constraint => return constraint.toString
      case obj: Product => obj.productPrefix
      case _ => obj.toString }

    var newString = ""
    newString = newString + s"$indent$prettyName$ptype\n"
//    println(s"$indent$prettyName$ptype")

    obj match {
      case seq: Iterable[Any] =>
        seq.foreach(elem =>
          newString = newString + pprintToString(elem, depth + 1))
      case obj: Product =>
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) =>
            newString = newString + pprintToString(subObj, depth + 1, Some(paramName))}
      case _ =>
    }

    return newString
  }
}
