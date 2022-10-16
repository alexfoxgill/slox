class LoxFunction(declaration: Stmt.Function) extends LoxCallable:
  def call(interpreter: Interpreter, arguments: List[Any]): Any = {
    val env = new Environment(Some(interpreter.globals))
    declaration.params.zip(arguments).foreach { (name, value) =>
      env.define(name.lexeme, value)
    }
    interpreter.execute(declaration.body, env)
  }
  def arity = declaration.params.length
  override def toString =
    s"<fn ${declaration.name.lexeme}>"
