#![allow(dead_code)]
#![feature(trace_macros)]

use pretty_env_logger;

mod lexer {
    use std::collections::VecDeque;

    use logos::{Lexer, Logos, Span, SpannedIter};

    type SourceSpan = Span;
    type Offset = usize;

    fn lex_integer<'lexer, 'input: 'lexer>(
        lex: &'lexer mut Lexer<'input, TokenKind<'input>>,
    ) -> Result<Constant, std::num::ParseIntError> {
        lex.slice().parse().map(Constant::Integral)
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
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

    #[derive(Debug, PartialEq)]
    pub enum Token<'input> {
        Indent,
        Dedent,
        EndLine,
        Raw {
            /// Kind of token this is
            token_kind: TokenKind<'input>,
            /// Byte range from the source text that this token was found at
            source_span: SourceSpan,
        },
    }

    impl<'a> Token<'a> {
        pub fn from_raw(kind: TokenKind<'a>, span: SourceSpan) -> Self {
            Self::Raw {
                token_kind: kind,
                source_span: span,
            }
        }
    }

    pub struct Tokenizer<'input> {
        raw_lexer: SpannedIter<'input, TokenKind<'input>>,
        token_buffer: VecDeque<Result<Token<'input>, LexicalError>>,
        indent_stack: Vec<WSKind>,
    }

    // TODO: Come up with better variant names
    #[derive(Debug, PartialEq)]
    pub enum LexicalError {
        MixedInlineIndentation,
        MixedInterlineIndetation,
    }

    impl<'input> Tokenizer<'input> {
        pub fn new(source: &'input str) -> Self {
            Self {
                raw_lexer: Lexer::new(source).spanned(),
                indent_stack: Default::default(),
                token_buffer: Default::default(),
            }
        }

        fn buffer_physical_line<'s>(&'s mut self) {
        }
    }

    impl<'input> Iterator for Tokenizer<'input> {
        type Item = Result<Token<'input>, LexicalError>;

        fn next<'s>(&'s mut self) -> Option<Self::Item> {
            if self.token_buffer.is_empty() {
                self.buffer_physical_line();
            }
            self.token_buffer.pop_front()
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        macro_rules! check_lexer_is_empty {
            ($lexer: expr) => {
                assert_eq!($lexer.next(), None)
            };
        }
        macro_rules! check_lexer_has_tokens {
            ($lexer: expr, $item: expr) => {
                assert_eq!($lexer.next(), Some(Ok($item)));
                check_lexer_is_empty!($lexer);
            };
            ($lexer: expr, $first:expr, $( $rest:expr ),+ $(,)?) => {
                assert_eq!($lexer.next(), Some(Ok($first)));
                check_lexer_has_tokens!($lexer, $($rest),+);
            };
        }

        #[test]
        fn tokenizes_empty_input() {
            check_lexer_is_empty!(Tokenizer::new(""));
        }

        #[test]
        fn treats_whitespace_line_as_empty_input() {
            check_lexer_is_empty!(Tokenizer::new("  \t\t\t"));
        }

        #[test]
        fn tokenizes_single_line() {
            let mut lexer = Tokenizer::new("varname: int = 12");
            check_lexer_has_tokens!(
                lexer,
                Token::from_raw(TokenKind::Ident("varname"), 0..7),
                Token::from_raw(TokenKind::Colon, 7..8),
                Token::from_raw(TokenKind::Ident("int"), 9..12),
                Token::from_raw(TokenKind::Assign, 13..14),
                Token::from_raw(TokenKind::BuiltinValue(Constant::Integral(12)), 15..17),
            );
        }

        #[test]
        fn tokenizes_leading_space_as_indent() {
            // Whitespace before start of code is treated as a indentation.
            // This is an error, that is caught by the parser
            let mut lexer = Tokenizer::new(" varname: int = 12");
            check_lexer_has_tokens!(
                lexer,
                Token::Indent,
                Token::from_raw(TokenKind::Ident("varname"), 1..8),
                Token::from_raw(TokenKind::Colon, 8..9),
                Token::from_raw(TokenKind::Ident("int"), 10..13),
                Token::from_raw(TokenKind::Assign, 14..15),
                Token::from_raw(TokenKind::BuiltinValue(Constant::Integral(12)), 16..18),
                Token::Dedent,
            );
        }
    }
}

use std::error::Error;

const SOURCE: &'static str = r##"
def is_zero ( items : [ int ] , idx : int ) -> bool :
    val : int = 0 # Type is explicitly declared
    val = items [ idx ]
    return val == 0

def always_true() -> bool:
    return (not True) == False

def syntax_error() -> bool:
    return not True == False

mylist : [ int ] = None
mylist = [1 , 10 , 999]
print( is_zero( mylist , 1) ) # Prints ’ True ’
"##;

fn main() -> Result<(), Box<dyn Error>> {
    pretty_env_logger::init();
    let lex = lexer::Tokenizer::new(SOURCE);

    for token in lex {
        println!("{:?}", token);
    }
    Ok(())
}
