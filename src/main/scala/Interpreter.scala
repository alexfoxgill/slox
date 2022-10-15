import scala.collection.mutable.HashMap

object Nada

class Environment(enclosing: Option[Environment] = None):
  private val values = HashMap.empty[String, Any]

  def define(name: String, value: Any): Unit =
    values += (name -> value)

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

class Interpreter(lox: Lox):
  private var environment = new Environment

  def interpret(statements: List[Stmt]): Any =
    try {
      statements.foreach(execute)
    } catch {
      case e: RuntimeError => lox.runtimeError(e)
    }

  private def execute(stmt: Stmt): Unit =
    stmt match
      case Stmt.Empty =>
        ()

      case Stmt.Expression(expr) =>
        evaluate(expr)
        ()

      case Stmt.Print(expr) =>
        val value = evaluate(expr)
        println(value)

      case Stmt.Var(name, initializer) =>
        val value: Any = initializer.map(evaluate).getOrElse(Nada)
        environment.define(name.lexeme, value)

      case Stmt.Block(statements) =>
        val prevEnv = environment
        try
          environment = new Environment(Some(environment))
          statements.foreach(execute)
        finally environment = prevEnv

      case Stmt.If(condition, thenBranch, elseBranch) =>
        if isTruthy(evaluate(condition)) then execute(thenBranch)
        else elseBranch.foreach(execute)

      case Stmt.While(condition, body) =>
        while isTruthy(evaluate(condition)) do execute(body)

  private def evaluate(expr: Expr): Any =
    expr match
      case Expr.Literal(value) => value
      case Expr.Grouping(expr) => evaluate(expr)
      case Expr.Var(name)      => environment.get(name)

      case Expr.Assign(name, value) =>
        environment.assign(name, evaluate(value))
        value

      case Expr.Unary(operator, right) =>
        (operator.typ, evaluate(right)) match
          case (TokenType.Minus, r: Double) => -r
          case (TokenType.Bang, r)          => !isTruthy(right)

          case (_, r) =>
            throw new RuntimeError(
              operator,
              s"Can't evaluate ${operator.lexeme} $r"
            )

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
          case (l, TokenType.BangEqual, r)                    => !l.equals(r)
          case (l, TokenType.EqualEqual, r)                   => l.equals(r)

          case (l, _, r) =>
            throw new RuntimeError(
              operator,
              s"Can't evaluate $l ${operator.lexeme} $r"
            )

      case Expr.Logical(left, operator, right) =>
        val l = evaluate(left)
        (isTruthy(l), operator.typ) match
          case (true, TokenType.And) | (false, TokenType.Or) => evaluate(right)
          case _                                             => l

  private def isTruthy(obj: Any) =
    obj != Nada && obj != false

class RuntimeError(val token: Token, message: String)
    extends RuntimeException(message)
