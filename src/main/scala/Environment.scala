import scala.collection.mutable.HashMap

class Environment(enclosing: Option[Environment] = None):
  private val values = HashMap.empty[String, LoxValue]

  def define(name: String, value: LoxValue): Environment =
    values += (name -> value)
    this

  def assign(name: Token, value: LoxValue): Environment =
    if values.contains(name.lexeme) then values += (name.lexeme -> value)
    else
      enclosing match
        case Some(env) => env.assign(name, value)
        case None =>
          throw new RuntimeError(
            name,
            s"Cannot assign to undefined variable '${name.lexeme}'"
          )
    this

  def assign(name: Token, value: LoxValue, depth: Int): Environment =
    ancestor(depth).values += (name.lexeme -> value)
    this

  def get(name: Token): LoxValue =
    values
      .get(name.lexeme)
      .orElse(enclosing.map(_.get(name)))
      .getOrElse {
        throw new RuntimeError(
          name,
          s"Cannot refer to undefined variable '${name.lexeme}'"
        )
      }

  def ancestor(depth: Int): Environment =
    (enclosing, depth) match
      case (_, 0)            => this
      case (Some(parent), _) => parent.ancestor(depth - 1)
      case _ =>
        throw new Exception(
          s"Expected to find parent environment at depth $depth"
        )

  def get(name: Token, depth: Int): LoxValue =
    ancestor(depth).values(name.lexeme)

  def createChild(): Environment = new Environment(Some(this))
