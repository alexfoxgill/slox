import scala.collection.mutable.HashMap

class LoxInstance(clas: LoxClass):
  private val fields = HashMap.empty[String, Any]

  override def toString = s"$clas instance"

  def get(name: Token): Any =
    fields
      .get(name.lexeme)
      .getOrElse {
        throw new RuntimeError(name, s"Undefined property '${name.lexeme}'")
      }

  def set(name: Token, value: Any): Unit =
    fields += name.lexeme -> value
