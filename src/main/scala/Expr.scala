enum Expr:
  case Literal(value: Any)
  case Binary(left: Expr, operator: Token, right: Expr)
  case Logical(left: Expr, operator: Token, right: Expr)
  case Unary(operator: Token, right: Expr)
  case Grouping(expr: Expr)
  case Var(id: Expr.Id, name: Token)
  case Assign(id: Expr.Id, name: Token, value: Expr)
  case Call(callee: Expr, arguments: List[Expr], closingParen: Token)

object Expr:
  opaque type Id = Int
  object Id:
    private var counter = 0
    def generate(): Id = {
      counter += 1
      counter
    }
