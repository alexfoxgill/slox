class LoxClass(name: String, methods: Map[String, LoxFunction])
    extends LoxRoot
    with LoxCallable:
  override def toString: String = name

  def call(interpreter: Interpreter, arguments: List[LoxValue]): LoxValue =
    new LoxInstance(this)

  def arity: Int = 0

  def findMethod(name: String): Option[LoxFunction] =
    methods.get(name)
