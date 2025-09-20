use litec_span::Span;

#[derive(Debug,PartialEq,Clone)]
pub struct Token<'src> {
    pub span: Span,
    pub kind: TokenKind,
    pub text: &'src str
}

impl<'src> Token<'src> {
    pub fn new(kind: TokenKind, span: Span, text: &'src str) -> Self{
        Token {
            span: span,
            kind: kind,
            text: text
        }
    }

    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.span.start()..self.span.end()-1]
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    LineComment,
    BlockComment,

    Ident,

    Literal {
        kind: LiteralKind,
        suffix: Option<String>,
    },

    /// `;`
    Semi,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `@`
    At,
    /// `#`
    Pound,
    /// `~`
    Tilde,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `$`
    Dollar,
    /// `=`
    Eq,
    /// `==`
    EqEq,
    /// `!=`
    NotEq,
    /// `!`
    Bang,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
    /// `-`
    Minus,
    /// `--`
    MinusMinus,
    /// `-=`
    MinusEq,
    /// `&`
    BitAnd,
    /// `&&`
    And,
    /// `|`
    BitOr,
    /// `||`
    Or,
    /// `+`
    Plus,
    /// `++`
    PlusPlus,
    /// `+=`
    PlusEq,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `^`
    Caret,
    /// `%`
    Percent,
    /// `%=`
    PercentEq,
    /// `->`
    Arrow,
    /// `=>`
    FatArrow,

    // Keyword
    Fn,
    Let,
    If,
    Else,
    While,
    For,
    Return,
    True,
    False,
    In,

    Error,
    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    Int {
        base: Base
    },

    Float {
        base: Base
    },

    Char {
        terminated: bool
    },

    Str {
        terminated: bool
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    Binary = 2,
    
    Octal = 8,
    
    Decimal = 10,
    
    Hexadecimal = 16
}