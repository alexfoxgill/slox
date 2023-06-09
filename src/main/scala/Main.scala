import java.io.BufferedReader
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths

@main def run(args: String*) =
  args match {
    case Seq()     => new Lox().runPrompt()
    case Seq(file) => new Lox().runFile(file)
    case _ =>
      println(s"Args: $args; Usage: jlox [script]")
      System.exit(64)
  }

class Lox {
  var hadError = false
  var hadRuntimeError = false

  def runPrompt(): Unit =
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    while true do {
      println("> ")
      val line = reader.readLine()
      if line == "" then {
        return ()
      }
      run(line)
    }

  def runFile(file: String) =
    val bytes = Files.readAllBytes(Paths.get(file))
    run(new String(bytes))

    if hadError then System.exit(65)
    if hadRuntimeError then System.exit(70)

  def run(code: String): Unit =
    val scanner = new Scanner(this, code)
    val tokens = scanner.scanTokens()

    val parser = new Parser(this, tokens.toVector)
    val statements = parser.parse()

    if hadError then return ()
    val interpreter = new Interpreter(this)
    val resolver = new Resolver(this, interpreter)
    resolver.resolve(statements)

    if hadError then return ()
    interpreter.interpret(statements)

  def error(line: Int, message: String) =
    report(line, "", message)

  def report(line: Int, where: String, message: String) =
    println(s"[line $line] Error $where: $message")
    hadError = true

  def error(token: Token, message: String) =
    val where =
      if token.typ == TokenType.EOF then "at end" else s"at '${token.lexeme}'"
    report(token.line, where, message)

  def runtimeError(error: RuntimeError) =
    println(error.getMessage)
    println(s"[line ${error.token.line}]")
    hadRuntimeError = true

  def print(message: String) =
    println(message)

}
