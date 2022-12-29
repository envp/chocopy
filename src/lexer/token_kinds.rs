use logos::{Lexer, Logos};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum WSKind {
    Tab(usize),
    Space(usize),
    Newline(usize),
}

impl PartialOrd for WSKind {
    /// Compare two whitespace objects.
    /// The comparison requires that they be the same 'kind' of whitespace.
    /// Returns the `Some` variant if successful, ordered by argument
    /// widths / character counts,
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Tab(s), Self::Tab(o)) => Some(s.cmp(o)),
            (Self::Space(s), Self::Space(o)) => Some(s.cmp(o)),
            (Self::Newline(s), Self::Newline(o)) => Some(s.cmp(o)),
            _ => None,
        }
    }
}

/// Primitive constants such as None (unit type), Booleans, and integers
/// are considered "builtins" in ChocoPy.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Boolean(bool),
    Integral(i32),
    NoneValue,
}

/// All the builtin operator symbols in ChocoPy
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Equal,
    NotEqual,
    Lesser,
    Greater,
    LesserEq,
    GreaterEq,
    Plus,
    Minus,
    Asterisk,
    Slash,
    SlashSlash,
    Modulus,
    And,
    Or,
    Not,
    In,
    Is,
}

/// All of ChocoPy's keywords. Keep these arranged alphabetically for
/// sanity.
#[derive(Debug, Clone, PartialEq)]
pub enum KW {
    As,
    Assert,
    Async,
    Await,
    Break,
    Class,
    Continue,
    Def,
    Del,
    Elif,
    Else,
    Except,
    Finally,
    For,
    From,
    Global,
    If,
    Import,
    Lambda,
    Nonlocal,
    Pass,
    Raise,
    Return,
    Try,
    While,
    With,
    Yield,
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

    #[token("==",  |_| Op::Equal)]
    #[token("!=",  |_| Op::NotEqual)]
    #[token("<",   |_| Op::Lesser)]
    #[token(">",   |_| Op::Greater)]
    #[token("<=",  |_| Op::LesserEq)]
    #[token(">=",  |_| Op::GreaterEq)]
    #[token("+",   |_| Op::Plus)]
    #[token("-",   |_| Op::Minus)]
    #[token("*",   |_| Op::Asterisk)]
    #[token("/",   |_| Op::Slash)]
    #[token("//",  |_| Op::SlashSlash)]
    #[token("%",   |_| Op::Modulus)]
    #[token("and", |_| Op::And)]
    #[token("or",  |_| Op::Or)]
    #[token("not", |_| Op::Not)]
    #[token("in",  |_| Op::In)]
    #[token("is",  |_| Op::Is)]
    Operator(Op),

    #[token("as",       priority = 2, callback = |_| KW::As)]
    #[token("assert",   priority = 2, callback = |_| KW::Assert)]
    #[token("async",    priority = 2, callback = |_| KW::Async)]
    #[token("await",    priority = 2, callback = |_| KW::Await)]
    #[token("break",    priority = 2, callback = |_| KW::Break)]
    #[token("class",    priority = 2, callback = |_| KW::Class)]
    #[token("continue", priority = 2, callback = |_| KW::Continue)]
    #[token("def",      priority = 2, callback = |_| KW::Def)]
    #[token("del",      priority = 2, callback = |_| KW::Del)]
    #[token("elif",     priority = 2, callback = |_| KW::Elif)]
    #[token("else",     priority = 2, callback = |_| KW::Else)]
    #[token("except",   priority = 2, callback = |_| KW::Except)]
    #[token("finally",  priority = 2, callback = |_| KW::Finally)]
    #[token("for",      priority = 2, callback = |_| KW::For)]
    #[token("from",     priority = 2, callback = |_| KW::From)]
    #[token("global",   priority = 2, callback = |_| KW::Global)]
    #[token("if",       priority = 2, callback = |_| KW::If)]
    #[token("import",   priority = 2, callback = |_| KW::Import)]
    #[token("lambda",   priority = 2, callback = |_| KW::Lambda)]
    #[token("nonlocal", priority = 2, callback = |_| KW::Nonlocal)]
    #[token("pass",     priority = 2, callback = |_| KW::Pass)]
    #[token("raise",    priority = 2, callback = |_| KW::Raise)]
    #[token("return",   priority = 2, callback = |_| KW::Return)]
    #[token("try",      priority = 2, callback = |_| KW::Try)]
    #[token("while",    priority = 2, callback = |_| KW::While)]
    #[token("with",     priority = 2, callback = |_| KW::With)]
    #[token("yield",    priority = 2, callback = |_| KW::Yield)]
    Keyword(KW),

    #[regex("(?&digit)+", lex_integer)]
    #[token("True", |_| Constant::Boolean(true))]
    #[token("False", |_| Constant::Boolean(false))]
    #[token("None", |_| Constant::NoneValue)]
    Literal(Constant),

    #[regex("(?&idstart)(?&idpart)*", priority = 1, callback = |lex| lex.slice())]
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

    #[inline]
    pub fn is_keyword(&self) -> bool {
        matches!(self, TokenKind::Keyword(_))
    }
}
