import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

enum IsReady:
  case Yes, No

enum ClassType:
  case None, Class

class StateStack[A](base: A):
  private var current = base

  def get: A = current

  def push[B](next: A)(block: => B): B =
    val prev = current
    current = next
    try
      block
    finally
      current = prev

class Resolver(lox: Lox, interpreter: Interpreter):
  private val scopes = Stack.empty[HashMap[String, IsReady]]
  private val functionStack = StateStack(FunctionType.None)
  private val classStack = StateStack(ClassType.None)

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
        functionStack.get match
          case FunctionType.None =>
            lox.error(keyword, "Can't return from top-level code")
          case FunctionType.Init if value.isDefined =>
            lox.error(keyword, "Can't return a value from an initializer")
          case _ => ()
        value.foreach(resolve)

      case Stmt.While(condition, body) =>
        resolve(condition)
        resolve(body)

      case Stmt.Expression(expr) =>
        resolve(expr)

      case Stmt.Class(name, superclass, methods) =>
        declare(name)
        define(name)
        superclass.foreach { s =>
          if name.lexeme == s.name.lexeme then
            lox.error(s.name, "A class can't inherit from itself")
          resolve(s)
        }
        classStack.push(ClassType.Class) {
          inScope {
            scopes.head += "this" -> IsReady.Yes
            methods.foreach { m =>
              val functionType = FunctionType.fromMethodName(m.name.lexeme)
              resolveFunction(m, functionType)
            }
          }
        }

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
      case Expr.Get(obj, name) =>
        resolve(obj)
      case Expr.Set(obj, name, value) =>
        resolve(obj)
        resolve(value)
      case Expr.This(id, name) =>
        if classStack.get == ClassType.None then
          lox.error(name, "Can't use 'this' outside class")
        resolveLocal(id, name)
      case Expr.Literal(_) => ()

  private def resolveFunction(
      function: Stmt.Function,
      functionType: FunctionType
  ) =
    functionStack.push(functionType) {
      inScope {
        function.params.foreach { p =>
          declare(p)
          define(p)
        }
        resolve(function.body)
      }
    }

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
