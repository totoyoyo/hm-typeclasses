package typeChecker

sealed class TypeCheckException(message: String) extends Exception(message)
case class NoVarException(s: String) extends TypeCheckException(s)
case class CannotUnify(s:String)  extends TypeCheckException(s)
