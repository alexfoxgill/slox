
class Interpreter:

  def interpret(expr: Expr): Any =
    try {
      evaluate(expr)
    } catch {
      case e: RuntimeError => Lox.runtimeError(e)
    }

  def evaluate(expr: Expr): Any =
    expr match
      case Expr.Literal(value) => value
      case Expr.Grouping(expr) => evaluate(expr)

      case Expr.Unary(operator, right) =>
        (operator.typ, evaluate(right)) match
          case (TokenType.Minus, r: Double) => -r
          case (TokenType.Bang, r)          => !isTruthy(right)
          case (_, r)                      => throw new RuntimeError(operator, s"Can't evaluate ${operator.lexeme} $r")

      case Expr.Binary(left, operator, right) =>
        (evaluate(left), operator.typ, evaluate(right)) match
          case (l: Double, TokenType.Minus, r: Double) => l - r
          case (l: Double, TokenType.Plus, r: Double) => l + r
          case (l: Double, TokenType.Star, r: Double) => l * r
          case (l: Double, TokenType.Slash, r: Double) => l / r
          case (l: String, TokenType.Plus, r: String) => l + r

          case (l: Double, TokenType.Greater, r: Double) => l > r
          case (l: Double, TokenType.GreaterEqual, r: Double) => l >= r
          case (l: Double, TokenType.Less, r: Double) => l < r
          case (l: Double, TokenType.LessEqual, r: Double) => l <= r

          case (l, TokenType.BangEqual, r) => !isEqual(l ,r)
          case (l, TokenType.EqualEqual, r) => isEqual(l, r)
          case (l, _, r) => throw new RuntimeError(operator, s"Can't evaluate $l ${operator.lexeme} $r")
        
  private def isTruthy(obj: Any) =
    obj != null && obj != false

  private def isEqual(a: Any, b: Any) =
    (a == null && b == null) || (a != null && a.equals(b))

class RuntimeError(val token: Token, message: String) extends RuntimeException(message)