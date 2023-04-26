package typeChecker
// Pretty printer adapted from https://stackoverflow.com/questions/15718506/scala-how-to-print-case-classes-like-pretty-printed-tree

object PPrinter {
  def pprint(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit = {
    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    obj match {
      case typ: Type =>
        println(s"$indent$prettyName" + typ.toString)
      case term: Term =>
        println(s"$indent$prettyName" + term.toString)
      case constraint: Constraint =>
        println(s"$indent$prettyName" + constraint.toString)
      case seq: Iterable[Any] =>
        println(s"$indent$prettyName")
        seq.foreach(pprint(_, depth + 1))
      case obj: Product =>
        println(s"$indent$prettyName" + obj.productPrefix)
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) => pprint(subObj, depth + 1, Some(paramName)) }
      case _ =>
        print(s"$indent$prettyName")
        println(obj.toString)
    }
  }
  def pprintToString(obj: Any, depth: Int = 0, paramName: Option[String] = None): String = {
    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    obj match {
      case typ: Type =>
        return s"$indent$prettyName" + typ.toString
      case term: Term =>
        return s"$indent$prettyName" + term.toString
      case constraint: Constraint =>
        return (s"$indent$prettyName" + constraint.toString)
      case seq: Iterable[Any] =>
        var newString = ""
        newString = newString + s"$indent$prettyName\n"
        seq.foreach(elem =>
          newString = newString + pprintToString(elem, depth + 1))
        return newString
      case obj: Product =>
        var newString = ""
        newString = newString + s"$indent$prettyName\n"
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) =>
            newString = newString + pprintToString(subObj, depth + 1, Some(paramName))}
        return newString
      case _ =>
        var newString = ""
        newString = newString + s"$indent$prettyName\n" + obj.toString
        return newString
    }
  }
}
