enum FunctionType:
  case None, Function, Method, Init

object FunctionType:
  def fromMethodName(name: String) =
    if name == "init" then FunctionType.Init
    else FunctionType.Method
