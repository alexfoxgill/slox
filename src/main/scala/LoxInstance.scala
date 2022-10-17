import scala.collection.mutable.HashMap

class LoxInstance(clas: LoxClass) extends LoxRoot:
  private val fields = HashMap.empty[String, LoxValue]

  override def toString = s"$clas instance"

  def get(name: Token): LoxValue =
    fields
      .get(name.lexeme)
      .orElse(clas.findMethod(name.lexeme).map(_.bind(this)))
      .getOrElse {
        throw new RuntimeError(
          name,
          s"Undefined property on $clas: '${name.lexeme}'"
        )
      }

  def set(name: Token, value: LoxValue): Unit =
    fields += name.lexeme -> value
