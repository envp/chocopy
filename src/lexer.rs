mod token_kinds;
use std::{cmp::Ordering, collections::VecDeque, iter::Peekable};

use logos::{Lexer, Span, SpannedIter};

pub use token_kinds::*;

use crate::utils::iter::PeekingTakeWhileExt;

/// Byte offset into a source file
type Offset = usize;

/// A pair of source file offsets that capture a range within a source file
///
/// TODO: Current we assume that all code lives in a single file. This type
/// needs to be coupled with some kind of "SourceManager" type to be useful.
type ByteSpan = (Offset, Offset);

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
    raw_lexer: Peekable<SpannedIter<'input, TokenKind<'input>>>,
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
            raw_lexer: Lexer::new(source).spanned().peekable(),
            indent_stack: Default::default(),
            token_buffer: Default::default(),
        }
    }

    /// Capture the tokens on up to the first newline token.
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
                let mixed_indent_error_handler = |_| Err(LexicalError::MixedInlineIndentation);
                let retrieve_indent_handler = || {
                    Ok(*current_ws.unwrap_or_else(|| {
                        unreachable!("Expected whitespace, but got: '{token:?}'")
                    }))
                };
                // If the next token is a whitespace, call `mixed_indent_error_handler`
                // otherwise, retrieve the indentation.
                let result = line
                    .next_if(|ts| ts.0.is_indentation())
                    .map_or_else(retrieve_indent_handler, mixed_indent_error_handler);
                line.peeking_take_while(|ts| ts.0.is_indentation())
                    .for_each(drop);
                result
            });
        if line.peek().is_some() {
            let token_buffer = &mut self.token_buffer;
            let indent_stack = &mut self.indent_stack;
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
            token_buffer.push_back(Ok(Token::EndLine));
        }
    }

    // This either returns a single indent token, or returns a sequence
    // of dedent tokens depending on the inter
    fn compute_indent_tokens(
        indent_stack: &mut Vec<WSKind>,
        indentation: Option<Result<WSKind, LexicalError>>,
    ) -> Result<Vec<Token<'input>>, LexicalError> {
        if let Some(Ok(current_indent)) = indentation {
            if let Some(last_indent) = indent_stack.last() {
                match current_indent.partial_cmp(last_indent) {
                    Some(Ordering::Less) => {
                        let idx = indent_stack.iter().rposition(|item| &current_indent > item);
                        let length = if let Some(i) = idx {
                            indent_stack.drain(i + 1..).count()
                        } else {
                            indent_stack.drain(..).count()
                        };
                        Ok(vec![Token::Dedent; length])
                    }
                    Some(Ordering::Greater) => Ok(vec![Token::Indent]),
                    Some(Ordering::Equal) => Ok(vec![]),
                    // TODO:
                    // Capture previous & current kinds/spans for
                    // better error reporting
                    None => Err(LexicalError::MixedInterlineIndetation),
                }
            } else {
                indent_stack.push(current_indent);
                Ok(vec![Token::Indent])
            }
        } else if let Some(Err(err)) = indentation {
            Err(err)
        } else {
            Ok(vec![])
        }
    }

    fn has_tokens_remaining(&mut self) -> bool {
        self.raw_lexer.peek().is_some()
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Token<'input>, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip past whitespace-only lines while buffering tokens
        while self.token_buffer.is_empty() && self.has_tokens_remaining() {
            self.buffer_physical_line();
        }

        // If the buffer is still empty, we've reached end of input.
        // Generate necessary dedent tokens
        self.token_buffer
            .pop_front()
            .or_else(|| self.indent_stack.pop().map(|_| Ok(Token::Dedent)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    macro_rules! check_lexer_is_empty {
        ($lexer: expr) => {
            assert_eq!(None, $lexer.next())
        };
    }
    macro_rules! check_lexer_has_tokens {
        ($lexer: expr, $item: expr) => {
            assert_eq!(Some(Ok($item)), $lexer.next());
            check_lexer_is_empty!($lexer);
        };
        ($lexer: expr, $first:expr, $( $rest:expr ),+ $(,)?) => {
            assert_eq!(Some(Ok($first)), $lexer.next());
            check_lexer_has_tokens!($lexer, $($rest),+);
        };
    }

    macro_rules! make_test_case {
        ($name: ident, $input: expr, []) => {
            #[test]
            fn $name() {
                check_lexer_is_empty!(Tokenizer::new($input));
            }
        };
        ($name: ident, $input: expr, $( $expected_tokens: expr ),+ $(,)?) => {
            #[test]
            fn $name() {
                let mut lexer = Tokenizer::new($input);
                check_lexer_has_tokens!(lexer, $($expected_tokens),+);
            }
        };
    }

    make_test_case!(empty_line, "", []);
    make_test_case!(empty_input, "\n   \n  \n", []);
    make_test_case!(treats_whitespace_line_as_empty_input, "  \t\t\t", []);
    make_test_case!(
        treats_multiple_whitespace_lines_as_empty_input,
        "  \t\t\t\n\t \t\n\n\n\t ",
        []
    );
    make_test_case!(
        tokenizes_comment_only_input,
        r##"#!/usr/bin/chocopy
            # This is another comment"##,
        Token::from_raw(TokenKind::Comment("#!/usr/bin/chocopy"), 0..18),
        Token::EndLine,
        Token::Indent,
        Token::from_raw(TokenKind::Comment("# This is another comment"), 31..56),
        Token::EndLine,
        Token::Dedent,
    );

    make_test_case!(
        tokenizes_single_line,
        "varname: int = 12",
        Token::from_raw(TokenKind::Ident("varname"), 0..7),
        Token::from_raw(TokenKind::Colon, 7..8),
        Token::from_raw(TokenKind::Ident("int"), 9..12),
        Token::from_raw(TokenKind::Assign, 13..14),
        Token::from_raw(TokenKind::Literal(Constant::Integral(12)), 15..17),
        Token::EndLine
    );

    make_test_case!(
        tokenizes_leading_space_as_indent,
        " varname: int = 12",
        Token::Indent,
        Token::from_raw(TokenKind::Ident("varname"), 1..8),
        Token::from_raw(TokenKind::Colon, 8..9),
        Token::from_raw(TokenKind::Ident("int"), 10..13),
        Token::from_raw(TokenKind::Assign, 14..15),
        Token::from_raw(TokenKind::Literal(Constant::Integral(12)), 16..18),
        Token::EndLine,
        Token::Dedent,
    );

    make_test_case!(
        simple_function,
        indoc!(
            r##"
        def add(x: int, y: int) -> int:
            return x + y
        "##
        ),
        Token::from_raw(TokenKind::Keyword(KW::Def), 0..3),
        Token::from_raw(TokenKind::Ident("add"), 4..7),
        Token::from_raw(TokenKind::LParen, 7..8),
        Token::from_raw(TokenKind::Ident("x"), 8..9),
        Token::from_raw(TokenKind::Colon, 9..10),
        Token::from_raw(TokenKind::Ident("int"), 11..14),
        Token::from_raw(TokenKind::Comma, 14..15),
        Token::from_raw(TokenKind::Ident("y"), 16..17),
        Token::from_raw(TokenKind::Colon, 17..18),
        Token::from_raw(TokenKind::Ident("int"), 19..22),
        Token::from_raw(TokenKind::RParen, 22..23),
        Token::from_raw(TokenKind::RArrow, 24..26),
        Token::from_raw(TokenKind::Ident("int"), 27..30),
        Token::from_raw(TokenKind::Colon, 30..31),
        Token::EndLine,
        Token::Indent,
        Token::from_raw(TokenKind::Keyword(KW::Return), 36..42),
        Token::from_raw(TokenKind::Ident("x"), 43..44),
        Token::from_raw(TokenKind::Operator(Op::Plus), 45..46),
        Token::from_raw(TokenKind::Ident("y"), 47..48),
        Token::EndLine,
        Token::Dedent,
    );
}
