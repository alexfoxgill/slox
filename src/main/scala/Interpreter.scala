import java.time.LocalDateTime
import scala.collection.mutable.HashMap

case class Return(value: LoxValue) extends Throwable

class Interpreter(lox: Lox):
  val globals = new Environment()
    .define(
      "clock",
      new LoxCallable {
        def arity = 0
        def call(interpreter: Interpreter, args: List[LoxValue]): LoxValue =
          LoxWrapper(System.currentTimeMillis / 1000.0)
        override def toString = "<native fn>"
      }
    )
  private var environment = globals

  private val locals = HashMap.empty[Expr.Id, Int]

  def interpret(statements: List[Stmt]): Unit =
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
        val value: LoxValue = initializer.map(evaluate).getOrElse(LoxNil)
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
        environment.define(
          name.lexeme,
          new LoxFunction(f, environment, FunctionType.Function)
        )

      case Stmt.Return(keyword, value) =>
        throw new Return(value.map(evaluate).getOrElse(LoxNil))

      case Stmt.Class(name, methods) =>
        // null is ok here because we immediately assign (we define before creation so it can refer to iteslf)
        environment.define(name.lexeme, null)
        environment.define("this", null)
        val clas = new LoxClass(
          name.lexeme,
          methods.map { m =>
            val methodType = FunctionType.fromMethodName(m.name.lexeme)
            m.name.lexeme -> new LoxFunction(m, environment, methodType)
          }.toMap
        )
        environment.assign(name, clas)

  private def evaluate(expr: Expr): LoxValue =
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
        rhs

      case Expr.Unary(operator, right) =>
        LoxWrapper(
          (operator.typ, evaluate(right)) match
            case (TokenType.Minus, r: Double) => -r
            case (TokenType.Bang, r)          => !isTruthy(r)

            case (_, r) =>
              throw new RuntimeError(
                operator,
                s"Can't evaluate ${operator.lexeme} $r"
              )
        )

      case Expr.Binary(left, operator, right) =>
        LoxWrapper(
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
        )

      case Expr.Logical(left, operator, right) =>
        val l = evaluate(left)
        (isTruthy(l), operator.typ) match
          case (true, TokenType.And) | (false, TokenType.Or) => evaluate(right)
          case _                                             => l

      case Expr.Call(callee, args, closingParen) =>
        val c = evaluate(callee)
        c match
          case c: LoxCallable =>
            if c.arity == args.length then c.call(this, args.map(evaluate))
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

      case Expr.Get(obj, name) =>
        evaluate(obj) match
          case instance: LoxInstance => instance.get(name)
          case _ =>
            throw new RuntimeError(name, "Only instances have properties")

      case Expr.Set(obj, name, value) =>
        val v = evaluate(value)
        evaluate(obj) match
          case instance: LoxInstance =>
            instance.set(name, v)
            v
          case _ =>
            throw new RuntimeError(name, "Only instances have properties")

      case Expr.This(id, name) =>
        locals
          .get(id)
          .map(depth => environment.get(name, depth))
          .getOrElse {
            throw new RuntimeError(
              name,
              "Cannot use 'this' outside class scope"
            )
          }

  private def stringify(value: LoxValue): String =
    value match
      case LoxNil    => "nil"
      case x: Double => x.toString.stripSuffix(".0")
      case _         => value.toString

  private def isTruthy(obj: LoxValue) =
    obj != LoxNil && obj != LoxWrapper(false)

class RuntimeError(val token: Token, message: String)
    extends RuntimeException(message)
