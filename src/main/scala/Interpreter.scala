import scala.collection.mutable.HashMap

class Environment(enclosing: Option[Environment] = None):
  private val values = HashMap.empty[String, Any]

  def define(name: String, value: Any): Unit =
    values += (name -> value)

  def assign(name: Token, value: Any): Unit =
    if values.contains(name.lexeme) then
      values += (name.lexeme -> value)
    else
      enclosing match
        case Some(env) => env.assign(name, value)
        case None => throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'")

  def get(name: Token): Any =
    values
      .get(name.lexeme)
      .orElse(enclosing.map(_.get(name)))
      .getOrElse { throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'") }

class Interpreter:
  private var environment = new Environment

  def interpret(statements: List[Stmt]): Any =
    try {
      statements.foreach(execute)
    } catch {
      case e: RuntimeError => Lox.runtimeError(e)
    }

  private def execute(stmt: Stmt): Unit =
    stmt match
      case Stmt.Expression(expr) =>
        evaluate(expr)
        ()

      case Stmt.Print(expr) =>
        val value = evaluate(expr)
        println(value)

      case Stmt.Var(name, initializer) =>
        val value: Any = if initializer != null then evaluate(initializer) else null
        environment.define(name.lexeme, value)

      case Stmt.Block(statements) =>
        val prevEnv = environment
        try
          environment = new Environment(Some(environment))
          statements.foreach(execute)
        finally
          environment = prevEnv


  private def evaluate(expr: Expr): Any =
    expr match
      case Expr.Literal(value) => value
      case Expr.Grouping(expr) => evaluate(expr)
      case Expr.Var(name) => environment.get(name)

      case Expr.Assign(name, value) =>
        environment.assign(name, evaluate(value))
        value

      case Expr.Unary(operator, right) =>
        (operator.typ, evaluate(right)) match
          case (TokenType.Minus, r: Double) => -r
          case (TokenType.Bang, r)          => !isTruthy(right)

          case (_, r) => throw new RuntimeError(operator, s"Can't evaluate ${operator.lexeme} $r")

      case Expr.Binary(left, operator, right) =>
        (evaluate(left), operator.typ, evaluate(right)) match
          case (l: Double, TokenType.Minus, r: Double)        => l - r
          case (l: Double, TokenType.Plus, r: Double)         => l + r
          case (l: Double, TokenType.Star, r: Double)         => l * r
          case (l: Double, TokenType.Slash, r: Double)        => l / r
          case (l: String, TokenType.Plus, r: String)         => l + r
          case (l: Double, TokenType.Greater, r: Double)      => l > r
          case (l: Double, TokenType.GreaterEqual, r: Double) => l >= r
          case (l: Double, TokenType.Less, r: Double)         => l < r
          case (l: Double, TokenType.LessEqual, r: Double)    => l <= r
          case (l, TokenType.BangEqual, r)                    => !isEqual(l ,r)
          case (l, TokenType.EqualEqual, r)                   => isEqual(l, r)

          case (l, _, r) => throw new RuntimeError(operator, s"Can't evaluate $l ${operator.lexeme} $r")
        
  private def isTruthy(obj: Any) =
    obj != null && obj != false

  private def isEqual(a: Any, b: Any) =
    (a == null && b == null) || (a != null && a.equals(b))

class RuntimeError(val token: Token, message: String) extends RuntimeException(message)