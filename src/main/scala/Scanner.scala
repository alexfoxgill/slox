import scala.collection.mutable.ArrayBuffer
import TokenType._
import Scanner._

class Scanner(source: String):
  private var start = 0
  private var current = 0
  private var line = 1
  private val tokens = ArrayBuffer.empty[Token]

  def scanTokens(): Iterable[Token] =
    while !isAtEnd do
      start = current
      scanToken()

    tokens += new Token(EOF, "", null, line)
    tokens.toList

  private def isAtEnd = current >= source.length

  private def scanToken() =
    advance() match
      case '(' => addToken(LeftParen)
      case ')' => addToken(RightParen)
      case '{' => addToken(LeftBrace)
      case '}' => addToken(RightBrace)
      case ',' => addToken(Comma)
      case '.' => addToken(Dot)
      case '-' => addToken(Minus)
      case '+' => addToken(Plus)
      case ';' => addToken(Semicolon)
      case '*' => addToken(Star)
      case '!' => addToken(if matches('=') then BangEqual else Bang)
      case '=' => addToken(if matches('=') then EqualEqual else Equal)
      case '<' => addToken(if matches('=') then LessEqual else Less)
      case '>' => addToken(if matches('=') then GreaterEqual else Greater)
      case '/' =>
        if matches('/') then
          while peek != '\n' && !isAtEnd do advance()
        else
          addToken(Slash)
      case ' ' | '\r' | '\t' => ()
      case '\n' => line += 1
      case '"' => string()
      case c if c.isDigit => number()
      case c if isAlpha(c) => identifier()
      case _ =>
        Lox.error(line, "Unexpected character")

  private def isAlpha(c: Char) = c.isLetter || c == '_'
  private def isAlphanumeric(c: Char) = c.isDigit || isAlpha(c)

  private def identifier() =
    while isAlphanumeric(peek) do advance()

    val text = source.substring(start, current)
    keywords.get(text) match
      case Some(kw) => addToken(kw)
      case None => addToken(Identifier)

  private def number() =
    while peek.isDigit do advance()

    if peek == '.' && peekNext.isDigit then {
      advance()
      while peek.isDigit do advance()
    }

    addToken(Number, source.substring(start, current).toDouble)

  private def string() =
    while peek != '"' && !isAtEnd do
      if peek == '\n' then {
        line += 1
      }
      advance()

    if isAtEnd then
      Lox.error(line, "Unterminated string")
    else
      advance()

      val value = source.substring(start + 1, current - 1)
      addToken(String, value)

  private def peek = if isAtEnd then '\u0000' else source.charAt(current)
  private def peekNext = if current + 1 >= source.length then '\u0000' else source.charAt(current + 1)

  private def matches(expected: Char): Boolean =
    if isAtEnd then false
    else if source.charAt(current) != expected then false
    else {
      current += 1
      true
    }

  private def advance(): Char =
    val c = source.charAt(current)
    current += 1
    c

  private def addToken(token: TokenType, literal: Any = null) =
    val text = source.substring(start, current)
    tokens += new Token(token, text, literal, line)

object Scanner:
  private val keywords = Map(
    "and" -> And,
    "class" -> Class,
    "else" -> Else,
    "false" -> False,
    "for" -> For,
    "fun" -> Fun,
    "if" -> If,
    "nil" -> Nil,
    "or" -> Or,
    "print" -> Print,
    "return" -> Return,
    "super" -> Super,
    "this" -> This,
    "true" -> True,
    "var" -> Var,
    "while" -> While,
  )