import TokenType._

class Parser(tokens: IndexedSeq[Token]) {
  private var current = 0

  def parse(): Expr =
    try {
      expression()
    } catch {
      case _: ParseError => null
    }

  private def expression(): Expr =
    equality()

  private def equality(): Expr =
    var expr = comparison()
    while matches(BangEqual, EqualEqual) do
      val operator = previous
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def comparison(): Expr =
    var expr = term()
    while matches(Greater, GreaterEqual, Less, LessEqual) do
      val operator = previous
      val right = term()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def term(): Expr =
    var expr = factor()
    while matches(Minus, Plus) do
      val operator = previous
      val right = factor()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def factor(): Expr =
    var expr = unary()
    while matches(Slash, Star) do
      val operator = previous
      val right = unary()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def unary(): Expr =
    if matches(Bang, Minus) then
      val operator = previous
      val right = unary()
      Expr.Unary(operator, right)
    else
      primary()

  private def primary(): Expr =
    if matches(False) then Expr.Literal(false)
    else if matches(True) then Expr.Literal(true)
    else if matches(Nil) then Expr.Literal(null)
    else if matches(Number, String) then Expr.Literal(previous.literal)
    else if matches(LeftParen) then
      val expr = expression()
      consume(RightParen, "Expect ')' after expression")
      Expr.Grouping(expr)
    else throw error(peek, "Expected expression")


  private def matches(types: TokenType*): Boolean =
    val res = types.exists(check)
    if res then {
      advance()
    }
    res

  private def check(typ: TokenType): Boolean =
    if isAtEnd then false else peek.typ == typ

  private def advance() =
    if !isAtEnd then {
      current += 1
    }
    previous

  private def isAtEnd = peek.typ == EOF
  private def peek = tokens(current)
  private def previous = tokens(current - 1)

  private def consume(typ: TokenType, message: String) =
    if check(typ) then advance() else throw error(peek, message)

  private def error(token: Token, message: String) =
    Lox.error(token, message)
    new ParseError
}

class ParseError extends RuntimeException