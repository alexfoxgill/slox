import TokenType._

import scala.collection.mutable.ArrayBuffer

enum Expr:
  case Literal(value: Any)
  case Binary(left: Expr, operator: Token, right: Expr)
  case Logical(left: Expr, operator: Token, right: Expr)
  case Unary(operator: Token, right: Expr)
  case Grouping(expr: Expr)
  case Var(name: Token)
  case Assign(name: Token, value: Expr)
  case Call(name: Expr, arguments: List[Expr], closingParen: Token)

enum Stmt:
  case Empty
  case Expression(expr: Expr)
  case Print(expr: Expr)
  case Var(name: Token, initializer: Option[Expr])
  case Block(statements: List[Stmt])
  case If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])
  case While(condition: Expr, body: Stmt)
  case Function(name: Token, params: List[Token], body: List[Stmt])

object Stmt:
  def block(statements: Stmt*): Block = new Block(statements.toList)

class Parser(lox: Lox, tokens: IndexedSeq[Token]) {
  private var current = 0

  def parse(): List[Stmt] =
    val statements = ArrayBuffer.empty[Stmt]
    while !isAtEnd do statements += declaration()
    statements.toList

  private def declaration(): Stmt =
    try
      if matches(Var) then varDeclaration()
      else if matches(Fun) then function("function")
      else statement()
    catch
      case _: ParseError =>
        synchronize()
        Stmt.Empty

  private def function(kind: String): Stmt =
    val name = consume(Identifier, s"Expected $kind name")
    consume(LeftParen, s"Expected '(' after $kind name")
    val params = ArrayBuffer.empty[Token]
    if !check(RightParen) then
      params += consume(Identifier, "Expected parameter name")
      while matches(Comma) do
        if params.size >= 255 then
          error(peek, "Can't have more than 255 parameters")
        params += consume(Identifier, "Expected parameter name")
    consume(RightParen, "Expected ')' after parameters")
    consume(LeftBrace, s"Expected '{' before $kind body")
    val body = block()
    Stmt.Function(name, params.toList, body)

  private def varDeclaration(): Stmt =
    val name = consume(Identifier, "Expected variable name")
    val init = if matches(Equal) then Some(expression()) else None
    consume(Semicolon, "Expected ';' after variable declaration")
    Stmt.Var(name, init)

  private def statement(): Stmt =
    if matches(Print) then printStatement()
    else if matches(If) then ifStatement()
    else if matches(While) then whileStatement()
    else if matches(For) then forStatement()
    else if matches(LeftBrace) then Stmt.Block(block())
    else expressionStatement()

  private def forStatement(): Stmt =
    consume(LeftParen, "Expected '(' after 'for'")
    val initializer: Stmt =
      if matches(Semicolon) then Stmt.Empty
      else if matches(Var) then varDeclaration()
      else expressionStatement()
    val condition =
      if !check(Semicolon) then expression() else Expr.Literal(true)
    consume(Semicolon, "Expected ';' after for loop condition")
    val increment: Stmt = Stmt.Expression(
      if !check(RightParen) then expression() else Expr.Literal(Nada)
    )
    consume(RightParen, "Expected ')' after for loop clauses")
    val body = statement()

    Stmt.block(
      initializer,
      Stmt.While(condition, Stmt.block(body, increment))
    )

  private def whileStatement(): Stmt =
    consume(LeftParen, "Expected '(' after 'while'")
    val condition = expression()
    consume(RightParen, "Expected ')' after while loop condition")
    val body = statement()
    Stmt.While(condition, body)

  private def ifStatement(): Stmt =
    consume(LeftParen, "Expected '(' after 'if'")
    val condition = expression()
    consume(RightParen, "Expected ')' after if condition")
    val thenBranch = statement()
    val elseBranch = if matches(Else) then Some(statement()) else None
    Stmt.If(condition, thenBranch, elseBranch)

  private def block(): List[Stmt] =
    val statements = ArrayBuffer.empty[Stmt]
    while !check(RightBrace) && !isAtEnd do statements += declaration()
    consume(RightBrace, "Expected '}' after block")
    statements.toList

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
    var expr = or()
    if matches(Equal) then
      val equals = previous
      val value = assignment()
      expr match
        case Expr.Var(name) =>
          return Expr.Assign(name, value)
        case _ =>
          error(equals, "Invalid assignment target")
    expr

  private def or(): Expr =
    var expr = and()
    while matches(Or) do
      val operator = previous
      val right = and()
      expr = Expr.Logical(expr, operator, right)
    expr

  private def and(): Expr =
    var expr = equality()
    while matches(And) do
      val operator = previous
      val right = equality()
      expr = Expr.Logical(expr, operator, right)
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
    else call()

  private def call(): Expr =
    var expr = primary()
    while matches(LeftParen) do expr = finishCall(expr)
    expr

  private def finishCall(callee: Expr): Expr =
    val args = ArrayBuffer.empty[Expr]
    if !check(RightParen) then
      args += expression()
      while matches(Comma) do
        if args.length >= 255 then
          error(peek, "Can't have more than 255 arguments")
        args += expression()
    val closingParen = consume(RightParen, "Expected ')' after arguments")
    Expr.Call(callee, args.toList, closingParen)

  private def primary(): Expr =
    if matches(False) then Expr.Literal(false)
    else if matches(True) then Expr.Literal(true)
    else if matches(Nil) then Expr.Literal(Nada)
    else if matches(Number, String) then Expr.Literal(previous.getValue)
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
    !isAtEnd && peek.typ == typ

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
    lox.error(token, message)
    new ParseError

  private def synchronize(): Unit =
    advance()
    while !isAtEnd do
      if previous.typ == Semicolon then return ()
      peek.typ match
        case Class | Fun | Var | For | If | While | Print | Return => return ()
        case _                                                     => advance()

}

class ParseError extends RuntimeException
