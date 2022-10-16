import java.time.LocalDateTime
import scala.collection.mutable.HashMap

case class Return(value: Any) extends Throwable

class Interpreter(lox: Lox):
  val globals = new Environment()
    .define(
      "clock",
      new LoxCallable {
        def arity = 0
        def call(interpreter: Interpreter, args: List[Any]): Any =
          System.currentTimeMillis / 1000.0
        override def toString = "<native fn>"
      }
    )
  private var environment = globals

  private val locals = HashMap.empty[Expr.Id, Int]

  def interpret(statements: List[Stmt]): Any =
    try statements.foreach(execute)
    catch case e: RuntimeError => lox.runtimeError(e)

  def execute(statements: List[Stmt], env: Environment) =
    val prevEnv = environment
    try
      environment = env
      interpret(statements)
    finally environment = prevEnv

  def resolve(id: Expr.Id, depth: Int): Unit =
    locals += id -> depth

  private def execute(stmt: Stmt): Unit =
    stmt match
      case Stmt.Empty =>
        ()

      case Stmt.Expression(expr) =>
        evaluate(expr)
        ()

      case Stmt.Print(expr) =>
        val value = evaluate(expr)
        lox.print(stringify(value))

      case Stmt.Var(name, initializer) =>
        val value: Any = initializer.map(evaluate).getOrElse(LoxNil)
        environment.define(name.lexeme, value)

      case Stmt.Block(statements) =>
        val prevEnv = environment
        try
          environment = new Environment(Some(environment))
          statements.foreach(execute)
        finally environment = prevEnv

      case Stmt.If(condition, thenBranch, elseBranch) =>
        if isTruthy(evaluate(condition)) then execute(thenBranch)
        else elseBranch.foreach(execute)

      case Stmt.While(condition, body) =>
        while isTruthy(evaluate(condition)) do execute(body)

      case f @ Stmt.Function(name, params, body) =>
        environment.define(name.lexeme, new LoxFunction(f, environment))

      case Stmt.Return(keyword, value) =>
        throw new Return(evaluate(value))

      case Stmt.Class(name, methods) =>
        environment.define(name.lexeme, null)
        val clas = new LoxClass(name.lexeme)
        environment.assign(name, clas)

  private def evaluate(expr: Expr): Any =
    expr match
      case Expr.Literal(value) => value
      case Expr.Grouping(expr) => evaluate(expr)
      case Expr.Var(id, name) =>
        locals
          .get(id)
          .map(depth => environment.get(name, depth))
          .getOrElse(globals.get(name))

      case Expr.Assign(id, name, value) =>
        val rhs = evaluate(value)
        locals.get(id) match
          case Some(depth) => environment.assign(name, rhs, depth)
          case _           => globals.assign(name, rhs)
        value

      case Expr.Unary(operator, right) =>
        (operator.typ, evaluate(right)) match
          case (TokenType.Minus, r: Double) => -r
          case (TokenType.Bang, r)          => !isTruthy(right)

          case (_, r) =>
            throw new RuntimeError(
              operator,
              s"Can't evaluate ${operator.lexeme} $r"
            )

      case Expr.Binary(left, operator, right) =>
        (evaluate(left), operator.typ, evaluate(right)) match
          case (l: Double, TokenType.Minus, r: Double)        => l - r
          case (l: Double, TokenType.Plus, r: Double)         => l + r
          case (l: Double, TokenType.Star, r: Double)         => l * r
          case (l: Double, TokenType.Slash, r: Double)        => l / r
          case (l: String, TokenType.Plus, r: String)         => l + r
          case (l: Double, TokenType.Greater, r: Double)      => l > r
          case (l: Double, TokenType.GreaterEqual, r: Double) => l >= r
          case (l: Double, TokenType.Less, r: Double)         => l < r
          case (l: Double, TokenType.LessEqual, r: Double)    => l <= r
          case (l, TokenType.BangEqual, r)                    => !l.equals(r)
          case (l, TokenType.EqualEqual, r)                   => l.equals(r)

          case (l, _, r) =>
            throw new RuntimeError(
              operator,
              s"Can't evaluate $l ${operator.lexeme} $r"
            )

      case Expr.Logical(left, operator, right) =>
        val l = evaluate(left)
        (isTruthy(l), operator.typ) match
          case (true, TokenType.And) | (false, TokenType.Or) => evaluate(right)
          case _                                             => l

      case Expr.Call(callee, args, closingParen) =>
        val c = evaluate(callee)
        val a = args.map(evaluate)
        c match
          case c: LoxCallable =>
            if c.arity == args.length then c.call(this, a)
            else
              throw new RuntimeError(
                closingParen,
                s"Expected ${c.arity} arguments but got ${args.length}"
              )
          case _ =>
            throw new RuntimeError(
              closingParen,
              "Can only call functions and classes"
            )

  private def stringify(value: Any): String =
    value match
      case LoxNil    => "nil"
      case x: Double => x.toString.stripSuffix(".0")
      case _         => value.toString

  private def isTruthy(obj: Any) =
    obj != LoxNil && obj != false

class RuntimeError(val token: Token, message: String)
    extends RuntimeException(message)
