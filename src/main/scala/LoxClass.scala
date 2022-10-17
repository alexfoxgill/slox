class LoxClass(name: String) extends LoxRoot with LoxCallable:
  override def toString: String = name

  def call(interpreter: Interpreter, arguments: List[LoxValue]): LoxValue =
    new LoxInstance(this)

  def arity: Int = 0
