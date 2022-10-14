import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

@main def run(args: String*) =
  args match {
    case Seq() => Lox.runPrompt()
    case Seq(file) => Lox.runFile(file)
    case _ =>
      println(s"Args: $args; Usage: jlox [script]")
      System.exit(64)
  }

object Lox {
  var hadError = false

  def runPrompt(): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    while true do {
      println("> ")
      val line = reader.readLine()
      if line == "" then {
        return
      }
      run(line)
    }
  }

  def runFile(file: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(file))
    run(new String(bytes))

    if hadError then {
      System.exit(65)
    }
  }

  def run(code: String): Unit = {
    val scanner = new Scanner(code)

    val tokens = scanner.scanTokens()

    tokens.foreach(println)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String): Unit = {
    println(s"[line $line] Error $where: $message")
    hadError = true
  }
}