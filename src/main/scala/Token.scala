case class Token(
    typ: TokenType,
    lexeme: String,
    literal: Option[LoxPrimitive],
    line: Int
):
  def getValue: LoxPrimitive | LoxNil.type =
    literal.getOrElse(LoxNil)

enum TokenType:
  case LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals
    Identifier,
    String,
    Number,

    // keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EOF
end TokenType
