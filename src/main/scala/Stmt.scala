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
