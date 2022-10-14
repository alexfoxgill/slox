enum Expr:
  case Literal(value: Any)
  case Binary(left: Expr, operator: Token, right: Expr)
  case Unary(operator: Token, right: Expr)
  case Grouping(expr: Expr)
  