#![allow(dead_code)]
#![feature(trace_macros)]

mod lexer {
    use std::collections::VecDeque;

    use logos::{Lexer, Logos, Span, SpannedIter};

    type Offset = usize;
    type ByteSpan = (Offset, Offset);

    fn lex_integer<'lexer, 'input: 'lexer>(
        lex: &'lexer mut Lexer<'input, TokenKind<'input>>,
    ) -> Result<Constant, std::num::ParseIntError> {
        lex.slice().parse().map(Constant::Integral)
    }

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
        /// Compare two whitespaces. The comparison requires that they be the
        /// same 'kind' of whitespace. If successful, they are ordered by their
        /// widths / character counts.
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

    #[derive(Debug, PartialEq, Clone)]
    pub enum Token<'input> {
        Indent,
        Dedent,
        EndLine,
        Raw {
            /// Kind of token this is
            token_kind: TokenKind<'input>,
            /// Byte range from the source text that this token was found at
            source_span: ByteSpan,
        },
    }

    impl<'a> Token<'a> {
        pub fn from_raw(kind: TokenKind<'a>, span: Span) -> Self {
            Self::Raw {
                token_kind: kind,
                source_span: (span.start, span.end),
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
        MixedInlineIndentation(ByteSpan),
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

        /// Run through the tokens on a single physical line. i.e. up to the
        /// first newline token
        fn buffer_physical_line(&mut self) {
            let line = &mut self
                .raw_lexer
                .by_ref()
                .take_while(|ts| !ts.0.is_newline())
                .peekable();
            // Determine the *potential* indentation of this line. This is the
            // width of whitespace from the start of the line to the first
            // non-indent character. In our case this MUST be a single token by
            // design. This can be None if there is no indentation or whitespace
            // at the start of a line.
            let indentation: Option<Result<WSKind, LexicalError>> =
                line.next_if(|ts| ts.0.is_indentation()).map(|(token, _)| {
                    let current_ws = token.try_into_whitespace();
                    // TODO:
                    // Capture the raw token kinds on either side of the error.
                    // This should lead to more precise error reporting on
                    // exactly how indentation was mixed. i.e
                    // <space> <tab> v/s <tab> <space>
                    let mixed_indent_error_handler = |(_, span): (_, Span)| {
                        Err(LexicalError::MixedInlineIndentation((span.start, span.end)))
                    };
                    let retrieve_indent_handler = || {
                        Ok(*current_ws.unwrap_or_else(|| {
                            unreachable!("Expected whitespace, but got: '{token:?}'")
                        }))
                    };
                    // If the next token is a whitespace, call `mixed_indent_error_handler`
                    // otherwise, retrieve the indentation.
                    line.next_if(|ts| ts.0.is_indentation())
                        .map_or_else(retrieve_indent_handler, mixed_indent_error_handler)
                });
            let token_buffer = &mut self.token_buffer;
            let indent_stack = &mut self.indent_stack;
            if line.peek().is_some() {
                // Process tokens inside the line
                let result = Tokenizer::compute_indent_tokens(indent_stack, indentation);
                match result {
                    Ok(mut tokens) => token_buffer.extend(tokens.drain(..).map(Ok)),
                    Err(err) => token_buffer.push_back(Err(err)),
                };
                token_buffer.extend(
                    line.filter(|ts| !ts.0.is_indentation())
                        .map(|(t, s)| Ok(Token::from_raw(t, s))),
                );
            } else {
                token_buffer.push_back(Ok(Token::EndLine));
            }
        }

        // This either returns a single indent token, or returns a sequence
        // of dedent tokens depending on the inter
        fn compute_indent_tokens(
            indent_stack: &mut Vec<WSKind>,
            indentation: Option<Result<WSKind, LexicalError>>,
        ) -> Result<Vec<Token>, LexicalError> {
            if indentation.is_none() {
                return Ok(Default::default());
            }
            let indentation = indentation.unwrap();

            match indentation {
                Ok(current_indent) => {
                    if indent_stack.is_empty() {
                        indent_stack.push(current_indent);
                        Ok(vec![Token::Indent])
                    } else {
                        let last_indent = indent_stack
                            .last()
                            .expect("Got unexpectedly empty indentation stack!");
                        if last_indent.is_same_kind(&current_indent) {
                            let mut tokens = vec![];
                            if &current_indent > last_indent {
                                tokens.push(Token::Indent);
                            } else if &current_indent < last_indent {
                                let idx =
                                    indent_stack.iter().rposition(|item| &current_indent > item);
                                let drain_iter = if let Some(i) = idx {
                                    indent_stack.drain(i + 1..)
                                } else {
                                    indent_stack.drain(..)
                                };
                                drain_iter.for_each(|_| tokens.push(Token::Dedent));
                            }
                            Ok(tokens)
                        } else {
                            // TODO:
                            // Capture previous & current kinds/spans for
                            // better error reporting
                            Err(LexicalError::MixedInterlineIndetation)
                        }
                    }
                }
                Err(err) => Err(err),
            }
        }
    }

    impl<'input> Iterator for Tokenizer<'input> {
        type Item = Result<Token<'input>, LexicalError>;

        fn next(&mut self) -> Option<Self::Item> {
            // Skip past whitespace-only lines while buffering tokens
            while self.token_buffer.is_empty() {
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
        fn tokenizes_empty_line() {
            check_lexer_is_empty!(Tokenizer::new(""));
        }

        #[test]
        fn tokenizes_empty_input() {
            check_lexer_is_empty!(Tokenizer::new("\n   \n  \n"));
        }

        #[test]
        fn treats_whitespace_line_as_empty_input() {
            check_lexer_is_empty!(Tokenizer::new("  \t\t\t"));
        }

        #[test]
        fn treats_multiple_whitespace_lines_as_empty_input() {
            check_lexer_is_empty!(Tokenizer::new("  \t\t\t\n\t \t\n\n\n\t "));
        }

        #[test]
        #[ignore = "not implemented yet"]
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
        #[ignore = "not implemented yet"]
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

const SOURCE: &str = r##"
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
        println!("{token:?}");
    }
    Ok(())
}
