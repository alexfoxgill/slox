trait LoxCallable extends LoxRoot:
  def call(interpreter: Interpreter, arguments: List[LoxValue]): LoxValue
  def arity: Int
