class LoxFunction(declaration: Stmt.Function, parentEnv: Environment)
    extends LoxCallable:
  def call(interpreter: Interpreter, arguments: List[Any]): Any = {
    val env = new Environment(Some(parentEnv))
    declaration.params.zip(arguments).foreach { (name, value) =>
      env.define(name.lexeme, value)
    }
    try
      interpreter.execute(declaration.body, env)
      LoxNil
    catch case Return(value) => value
  }
  def arity = declaration.params.length
  override def toString =
    s"<fn ${declaration.name.lexeme}>"
