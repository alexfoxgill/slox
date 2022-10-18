class LoxFunction(
    declaration: Stmt.Function,
    closure: Environment,
    functionType: FunctionType
) extends LoxRoot
    with LoxCallable:
  def call(interpreter: Interpreter, arguments: List[LoxValue]): LoxValue = {
    val env = new Environment(Some(closure))
    declaration.params.zip(arguments).foreach { (name, value) =>
      env.define(name.lexeme, value)
    }
    try
      interpreter.execute(declaration.body, env)
      if functionType == FunctionType.Init then getThis
      else LoxNil
    catch
      case Return(value) =>
        if functionType == FunctionType.Init then getThis
        else value
  }
  def arity = declaration.params.length

  override def toString =
    s"<fn ${declaration.name.lexeme}>"

  def bind(instance: LoxInstance) =
    val env = closure.createChild()
    env.define("this", instance)
    new LoxFunction(declaration, env, functionType)

  private def getThis = closure.get("this", 0)
