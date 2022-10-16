import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

enum IsReady:
  case Yes, No

class Resolver(lox: Lox, interpreter: Interpreter):
  private val scopes = Stack.empty[HashMap[String, IsReady]]

  def resolve(statements: List[Stmt]): Unit =
    statements.foreach(resolve)

  def resolve(stmt: Stmt): Unit =
    stmt match
      case Stmt.Block(statements) =>
        inScope {
          resolve(statements)
        }

      case Stmt.Var(name, init) =>
        declare(name)
        init.foreach(resolve)
        define(name)

      case f @ Stmt.Function(name, _, _) =>
        declare(name)
        define(name)
        resolveFunction(f)

      case Stmt.If(condition, thenBranch, elseBranch) =>
        resolve(condition)
        resolve(thenBranch)
        elseBranch.foreach(resolve)

      case Stmt.Print(expr) =>
        resolve(expr)

      case Stmt.Return(_, value) =>
        resolve(value)

      case Stmt.While(condition, body) =>
        resolve(condition)
        resolve(body)

      case Stmt.Expression(expr) =>
        resolve(expr)

      case Stmt.Empty =>
        ()

  def resolve(expression: Expr): Unit =
    expression match
      case Expr.Var(id, name) =>
        if scopes.nonEmpty && scopes.head.get(name.lexeme).contains(IsReady.No)
        then
          lox.error(
            name,
            "Can't read local variable in its own initializer"
          )
        resolveLocal(id, name)
      case Expr.Binary(left, _, right) =>
        resolve(left)
        resolve(right)
      case Expr.Logical(left, _, right) =>
        resolve(left)
        resolve(right)
      case Expr.Unary(_, right) =>
        resolve(right)
      case Expr.Grouping(expr) =>
        resolve(expr)
      case Expr.Assign(id, name, value) =>
        resolve(value)
        resolveLocal(id, name)
      case Expr.Call(callee, arguments, _) =>
        resolve(callee)
        arguments.foreach(resolve)
      case _: Expr.Literal => ()

  private def resolveFunction(function: Stmt.Function) =
    inScope {
      function.params.foreach { p =>
        declare(p)
        define(p)
      }
      resolve(function.body)
    }

  private def resolveLocal(id: Expr.Id, name: Token) =
    val depth = scopes
      .indexWhere(_.contains(name.lexeme))

    if depth != -1 then interpreter.resolve(id, depth)

  private def declare(name: Token) =
    if scopes.nonEmpty then scopes.head += name.lexeme -> IsReady.No

  private def define(name: Token) =
    if scopes.nonEmpty then scopes.head += name.lexeme -> IsReady.Yes

  private def inScope(block: => Unit): Unit =
    scopes.push(HashMap.empty)
    block
    scopes.pop()
