enum Expr:
  case Literal(value: Any)
  case Binary(left: Expr, operator: Token, right: Expr)
  case Logical(left: Expr, operator: Token, right: Expr)
  case Unary(operator: Token, right: Expr)
  case Grouping(expr: Expr)
  case Var(name: Token)
  case Assign(name: Token, value: Expr)
  case Call(name: Expr, arguments: List[Expr], closingParen: Token)
