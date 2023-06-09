class LoxClass(
    name: String,
    superclass: Option[LoxClass],
    methods: Map[String, LoxFunction]
) extends LoxRoot
    with LoxCallable:
  override def toString: String = name

  def call(interpreter: Interpreter, arguments: List[LoxValue]): LoxValue =
    val instance = new LoxInstance(this)
    init.foreach(_.bind(instance).call(interpreter, arguments))
    instance

  def arity: Int = init.fold(0)(_.arity)

  def findMethod(name: String): Option[LoxFunction] =
    methods
      .get(name)
      .orElse(superclass.flatMap(_.findMethod(name)))

  private def init = findMethod("init")
