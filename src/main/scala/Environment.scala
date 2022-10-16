import scala.collection.mutable.HashMap

class Environment(enclosing: Option[Environment] = None):
  private val values = HashMap.empty[String, Any]

  def define(name: String, value: Any): Environment =
    values += (name -> value)
    this

  def assign(name: Token, value: Any): Unit =
    if values.contains(name.lexeme) then values += (name.lexeme -> value)
    else
      enclosing match
        case Some(env) => env.assign(name, value)
        case None =>
          throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'")

  def get(name: Token): Any =
    values
      .get(name.lexeme)
      .orElse(enclosing.map(_.get(name)))
      .getOrElse {
        throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'")
      }
