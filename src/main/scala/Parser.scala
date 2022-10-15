import scala.collection.mutable.ArrayBuffer

import TokenType._

enum Expr:
  case Literal(value: Any)
  case Binary(left: Expr, operator: Token, right: Expr)
  case Unary(operator: Token, right: Expr)
  case Grouping(expr: Expr)
  case Var(name: Token)
  case Assign(name: Token, value: Expr)
  
enum Stmt:
  case Expression(expr: Expr)
  case Print(expr: Expr)
  case Var(name: Token, initializer: Expr)
  case Block(statements: List[Stmt])

class Parser(tokens: IndexedSeq[Token]) {
  private var current = 0

  def parse(): List[Stmt] =
    val statements = ArrayBuffer.empty[Stmt]
    while !isAtEnd do
      statements += declaration()
    statements.toList

  private def declaration(): Stmt =
    try
      if matches(Var) then varDeclaration()
      else statement()
    catch
      case _: ParseError =>
        synchronize()
        null

  private def varDeclaration(): Stmt =
    val name = consume(Identifier, "Expected variable name")
    var initializer: Expr = null
    if matches(Equal) then
      initializer = expression()
    
    consume(Semicolon, "Expected ';' after variable declaration")
    Stmt.Var(name, initializer)

  private def statement(): Stmt =
    if matches(Print) then printStatement()
    else if matches(LeftBrace) then block()
    else expressionStatement()

  private def block(): Stmt =
    val statements = ArrayBuffer.empty[Stmt]
    while !check(RightBrace) && !isAtEnd do
      statements += declaration()
    consume(RightBrace, "Expected '}' after block")
    Stmt.Block(statements.toList)

  private def printStatement(): Stmt =
    val value = expression()
    consume(Semicolon, "Expected ';' after value")
    Stmt.Print(value)

  private def expressionStatement(): Stmt =
    val value = expression()
    consume(Semicolon, "Expected ';' after expression")
    Stmt.Expression(value)

  private def expression(): Expr =
    assignment()

  private def assignment(): Expr =
    var expr = equality()
    if matches(Equal) then
      val equals = previous
      val value = assignment()
      expr match
        case Expr.Var(name) =>
          return Expr.Assign(name, value)
        case _ =>
          error(equals, "Invalid assignment target")
    expr
      
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
    else if matches(Identifier) then Expr.Var(previous)
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

  private def synchronize(): Unit =
    advance()
    while !isAtEnd do
      if previous.typ == Semicolon then return ()
      peek.typ match
        case Class | Fun | Var | For | If | While | Print | Return => return ()
        case _ => advance()
      
}

class ParseError extends RuntimeException