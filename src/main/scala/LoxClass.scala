class LoxClass(name: String) extends LoxCallable:
  override def toString: String = name

  def call(interpreter: Interpreter, arguments: List[Any]): Any =
    new LoxInstance(this)

  def arity: Int = 0
