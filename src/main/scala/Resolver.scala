import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

enum IsReady:
  case Yes, No

enum FunctionType:
  case None, Function

class Resolver(lox: Lox, interpreter: Interpreter):
  private val scopes = Stack.empty[HashMap[String, IsReady]]
  private var currentFunction = FunctionType.None

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
        resolveFunction(f, FunctionType.Function)

      case Stmt.If(condition, thenBranch, elseBranch) =>
        resolve(condition)
        resolve(thenBranch)
        elseBranch.foreach(resolve)

      case Stmt.Print(expr) =>
        resolve(expr)

      case Stmt.Return(keyword, value) =>
        if currentFunction == FunctionType.None then
          lox.error(keyword, "Can't return from top-level code")
        resolve(value)

      case Stmt.While(condition, body) =>
        resolve(condition)
        resolve(body)

      case Stmt.Expression(expr) =>
        resolve(expr)

      case Stmt.Class(name, methods) =>
        declare(name)
        define(name)

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
      case Expr.Literal(_) => ()

  private def resolveFunction(
      function: Stmt.Function,
      functionType: FunctionType
  ) =
    var prevFunction = currentFunction
    try
      currentFunction = functionType
      inScope {
        function.params.foreach { p =>
          declare(p)
          define(p)
        }
        resolve(function.body)
      }
    finally currentFunction = prevFunction

  private def resolveLocal(id: Expr.Id, name: Token) =
    val depth = scopes
      .indexWhere(_.contains(name.lexeme))

    if depth != -1 then interpreter.resolve(id, depth)

  private def declare(name: Token) =
    scopes.headOption.foreach { scope =>
      if scope.contains(name.lexeme) then
        lox.error(name, "Already a variable with this name in this scope")
      scope += name.lexeme -> IsReady.No
    }

  private def define(name: Token) =
    scopes.headOption.foreach(_ += name.lexeme -> IsReady.Yes)

  private def inScope(block: => Unit): Unit =
    scopes.push(HashMap.empty)
    try
      block
    finally
      scopes.pop()
