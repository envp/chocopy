use logos::{Lexer, Logos};

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy)]
pub enum WSKind {
    Tab(usize),
    Space(usize),
    Newline(usize),
}

impl WSKind {
    pub fn char_count(&self) -> usize {
        let inner = match self {
            Self::Tab(size) => size,
            Self::Space(size) => size,
            Self::Newline(size) => size,
        };
        *inner
    }

    pub fn is_same_kind(&self, other: &WSKind) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl Ord for WSKind {
    /// Compare two whitespace objects.
    /// The comparison requires that they be the same 'kind' of whitespace.
    /// If successful, they are ordered by their widths / character counts.
    ///
    /// # Panics
    /// If the two whitespace are of a different kind, this will panic.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Tab(s), Self::Tab(o)) => s.cmp(o),
            (Self::Space(s), Self::Space(o)) => s.cmp(o),
            (Self::Newline(s), Self::Newline(o)) => s.cmp(o),
            _ => unreachable!("Cannot compare different whitespace kinds."),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Boolean(bool),
    Integral(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinBoolOp {
    Equal,
    NotEqual,
    Lesser,
    Greater,
    LesserEq,
    GreaterEq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinArithmeticOp {
    Plus,
    Minus,
    Asterisk,
    Slash,
    SlashSlash,
    Modulus,
}

fn lex_integer<'lexer, 'input: 'lexer>(
    lex: &'lexer mut Lexer<'input, TokenKind<'input>>,
) -> Result<Constant, std::num::ParseIntError> {
    lex.slice().parse().map(Constant::Integral)
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(subpattern idstart = r"[a-zA-Z_]")]
#[logos(subpattern digit = r"[0-9]")]
#[logos(subpattern idpart = r"((?&idstart)|(?&digit))")]
pub enum TokenKind<'input> {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("=")]
    Assign,
    #[token("->")]
    RArrow,

    #[token("==", |_| BuiltinBoolOp::Equal)]
    #[token("!=", |_| BuiltinBoolOp::NotEqual)]
    #[token("<", |_| BuiltinBoolOp::Lesser)]
    #[token(">", |_| BuiltinBoolOp::Greater)]
    #[token("<=", |_| BuiltinBoolOp::LesserEq)]
    #[token(">=", |_| BuiltinBoolOp::GreaterEq)]
    BoolOperator(BuiltinBoolOp),

    #[token("+", |_| BuiltinArithmeticOp::Plus)]
    #[token("-", |_| BuiltinArithmeticOp::Minus)]
    #[token("*", |_| BuiltinArithmeticOp::Asterisk)]
    #[token("/", |_| BuiltinArithmeticOp::Slash)]
    #[token("//", |_| BuiltinArithmeticOp::SlashSlash)]
    #[token("%", |_| BuiltinArithmeticOp::Modulus)]
    ArithmeticOperator(BuiltinArithmeticOp),

    #[token("def", priority = 2)]
    KWDef,
    #[token("return", priority = 2)]
    KWReturn,

    #[token("True", |_| Constant::Boolean(true))]
    #[token("False", |_| Constant::Boolean(false))]
    #[regex("(?&digit)+", lex_integer)]
    BuiltinValue(Constant),

    #[regex("(?&idstart)(?&idpart)+", priority = 1, callback = |lex| lex.slice())]
    Ident(&'input str),

    #[regex(r"\t+", |lex| {
        WSKind::Tab(lex.slice().len())
    })]
    #[regex(r" +", |lex| {
        WSKind::Space(lex.slice().len())
    })]
    #[regex(r"\n+", |lex| {
        WSKind::Newline(lex.slice().len())
    })]
    Whitespace(WSKind),

    #[regex(r"#.*", callback = |lex| lex.slice())]
    Comment(&'input str),

    #[error]
    Error,
}

impl<'input> TokenKind<'input> {
    #[inline]
    pub fn is_indentation(&self) -> bool {
        matches!(
            self,
            TokenKind::Whitespace(WSKind::Space(_)) | TokenKind::Whitespace(WSKind::Tab(_))
        )
    }

    #[inline]
    pub fn is_newline(&self) -> bool {
        matches!(self, TokenKind::Whitespace(WSKind::Newline(_)))
    }

    #[inline]
    pub fn is_whitespace(&self) -> bool {
        matches!(self, Self::Whitespace(_))
    }

    #[inline]
    pub fn try_into_whitespace(&self) -> Option<&WSKind> {
        match self {
            TokenKind::Whitespace(wskind) => Some(wskind),

            _ => None,
        }
    }
}
